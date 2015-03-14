; Program: bicycle.asm: 
; Author: Katja Barnhart & Austin Carroll
; Date: 12/6/2013
; Runs in 16-bit real [mode]. Made with Irvine's make16 batch file.

include irvine16.inc

.data
	circumference word 100 ;standard
	km dword 0
	kmh word 0
	maxSpeed word 96d
	mode byte 0 ; speed, odo, max speed
	M_E byte 0b ; Metric/English
	timerold dword 0
	timernew dword 0
	wheel byte 0
	rotatespeed word 60000
	rotatecounter word 1
.code

main proc
	mov ax, @data
	mov ds, ax
	mov ax, 0012h
	int 10h
	mov ax, 0a000h
	mov es, ax
	call circgui
mainLoop:
	mov di, 80 * 3
	call wheelrotate
	mov ax, 0600h
	mov dl, 0FFh
	int 21h ;will return 0 in AL if no character, FFh if a character is available
	jnz getchar
	;call writeax

	call writemode
	jmp mainLoop

	getchar:
		cmp al, 01bh ; esc key
		je escape
		cmp al, 020h ; space bar
		je spacebar
		cmp al, 075h ; u key
		je ukey
		cmp al, 06Dh ; m key
		je mkey
		jmp mainLoop
		
		spacebar:
			;calculate 
			movzx ebx, [circumference]
			add [km], ebx 
			call calculateSpeed
			jmp mainLoop
		
		mkey:
			;cycle units
			mov cx, 20000
			keyheld:
				int 21h
				cmp al, 06Dh
				jne notheld
				loop keyheld
				call circgui
			notheld:
				xor [M_E], 1
			jmp mainLoop
			
		ukey:
			;cycle display type
			call clearscreen
			inc [mode]
			cmp [mode], 3
			jne mainloop
			mov [mode], 0
			jmp mainLoop
			
	jmp mainLoop
	
escape:
	call clearscreen
	exit
main endp
;////////////////////////////////////////////////////
; Description: Rotates wheel on screen
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
wheelrotate proc
	pushad
	inc [rotatecounter]
	mov ax, [rotatespeed]
	cmp ax, [rotatecounter]
	jg ENDER
	mov [rotatecounter], 0
	mov al, [wheel]
	xor al, 1
	mov [wheel], al
	cmp al, 0
	mov di, (80 * 99) + 4
	je wheel2
	call wheelrotate1
	add di, 5
	call wheelrotate1
	jmp ENDER
wheel2:
	call wheelrotate2
	add di, 5
	call wheelrotate2
ENDER:
	popad
	ret
wheelrotate endp
;////////////////////////////////////////////////////
; Description: calculates speed based on time difference
;		   in [timerold] and [timernew]
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
calculateSpeed proc
	pushad
	mov ax, [circumference]
	mov bx, 36
	mul bx
	movzx ecx, ax
	call grabClock
	mov eax, [timernew]
	cmp eax, [timerold]
	jne notequal
	mov eax, 20 ;maximum speed for the program
	jmp jump
notequal:
	sub eax, [timerold]
	mov ebx, 55
	mul ebx
jump:
	xchg ecx, eax
	mov edx, 0
	div ecx
	mov [kmh], ax
	cmp ax, [maxSpeed]
	jl end1
	mov [maxSpeed], ax
end1:
	mov eax, 10000
	mov edx, 0
	mov bx, [kmh]
	cmp bx, 0
	je equal
	div bx
	mov [rotatespeed], ax
	jmp good
equal:
	mov [rotatespeed], 60000
good:
	popad
	ret
calculateSpeed endp
;////////////////////////////////////////////////////
; Description: Moves into circumference GUI and gets the 
;		   number for the circumference of wheel from user
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
circgui proc
	pushad
	call clearscreen
	mov di, 3*80
	call writecircumference
	add di, 4 * 16
	call writeupdown
	sub di, 4 * 3
	mov [circumference], 100
incdec:
	mov ax, [circumference]
	call writeaxnodot
	mov ax, 0600h
	mov dl, 0FFh
	int 21h ;will return 0 in AL if no character, FFh if a character is available
	je incdec
	cmp al, 01bh ; esc key
	je escape
	cmp al, 00Dh ; enter key
	je escape
	int 21h
	cmp al, 048h; up arrow
	je up
	cmp al, 050h; down arrow
	je down
	jmp incdec
up:
	add [circumference], 10
	jmp incdec
down:
	sub [circumference], 10
	jmp incdec
escape:
	call clearscreen
	popad
	ret
circgui endp
;////////////////////////////////////////////////////
; Description: updates clock values in [timerold] and [timernew]
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
grabClock proc
        ;grabs the clock tick!
        push eax
        push edx
        push es
        ;saves the previous clock tick into the old clock tick.
        mov eax, [timernew]
        mov [timerold], eax
        mov dx, 0040h
        mov es, dx
        mov eax, es:[006ch]
        mov [timernew], eax
        pop es
        pop edx
        pop eax
	ret
grabClock endp
;////////////////////////////////////////////////////
; Description: Converts cm in [km] to KM/10
; Receives: screen position in Di
; Returns: KM/10 in AX
; Requires: N/A
; Clobbers: EAX, EBX, EDX
;////////////////////////////////////////////////////
getDistanceKm proc
	mov eax, [km]
	mov ebx, 10000
	mov edx, 0
	div ebx
	ret
getDistanceKm endp
;////////////////////////////////////////////////////
; Description: Converts cm to Mi/10
; Receives: screen position in Di
; Returns: mi/10 in AX
; Requires: N/A
; Clobbers: EAX, EBX, EDX
;////////////////////////////////////////////////////
getDistanceMi proc
	mov eax, [km]
	mov ebx, 50
	mul ebx
	mov ebx, 804672
	mov edx, 0
	div ebx
	ret
getDistanceMi endp
;////////////////////////////////////////////////////
; Description: Converts kmh to MPH/10
; Receives: screen position in Di
; Returns: MPH/10 in AX
; Requires: N/A
; Clobbers: EAX, EBX, EDX
;////////////////////////////////////////////////////
getSpeedMi proc
	movzx eax, [kmh]
	mov ebx, 62
	mul ebx
	mov edx, 0
	mov ebx, 100
	div ebx
	ret
getSpeedMi endp
;////////////////////////////////////////////////////
; Description: Converts maxspeed to MPH/10
; Receives: screen position in Di
; Returns: MPH/10 in AX
; Requires: N/A
; Clobbers: EAX, EBX, EDX
;////////////////////////////////////////////////////
getMaxSpeedMi proc
	movzx eax, [maxSpeed]
	mov ebx, 62
	mul ebx
	mov edx, 0
	mov ebx, 100
	div ebx
	ret
getMaxSpeedMi endp
;////////////////////////////////////////////////////
; Description: Writes "circumference" to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
Writecircumference proc
	pushad
	call writec
	add di, 4
	call writei
	add di, 4
	call writer
	add di, 4
	call writec
	add di, 4
	call writeu
	add di, 4
	call writem
	add di, 4
	call writef
	add di, 4
	call writee
	add di, 4
	call writer
	add di, 4
	call writee
	add di, 4
	call writen
	add di, 4
	call writec
	add di, 4
	call writee
	popad
	ret
writecircumference endp

;////////////////////////////////////////////////////
; Description: Writes number to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
writeAXNoDot proc
	pushad
	mov cx, 0
	mov dx, 0
	mov si, 100
	div si
	cmp ax, 0
	je skip2
	call subwriteAX
	inc cx
	jmp noskip2
skip2:
	call writespace
noskip2:
	add di, 4
	mov ax, dx
	mov dx, 0
	mov si, 10
	div si
	cmp cx, 0
	jne nocomparison2
	cmp ax, 0
	jz skip3
	inc cx
nocomparison2:
	call subwriteAX
	jmp noskip3
skip3:
	call writespace
noskip3:
	add di, 4
	mov ax, dx
	call subwriteAX
	popad
	ret
writeAXNoDot  endp

;////////////////////////////////////////////////////
; Description: Writes All graphics to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
writemode proc
	pushad
	cmp [[mode]], 0
	je odo
	cmp [[mode]], 1
	je spd
	cmp [[mode]], 2
	je max
	popad
	ret
	
	odo:					;odometer [mode]
		call writezero
		add di, 4
		call writed
		add di, 4
		call writezero
		add di, (80 * 35) - 8
		cmp [M_E], 0
		je ometric
		jmp oenglish
		ometric:
			call writek
			add di, 4
			call writem
			add di, 4
			call getDistanceKm
		jmp next
		oenglish:
			call writem                 
			add di, 4
			call writei
			add di, 4
			call getDistanceMi
		jmp next
	spd:					;speed [mode]
		call writes
		add di, 4
		call writep
		add di, 4
		call writed
		add di, (80 * 35) - 8
		cmp [M_E], 0
		je smetric
		jmp senglish
		smetric:
			call writek
			add di, 4
			call writep
			add di, 4
			call writeh
			add di, 4
			mov ax, [kmh]
		jmp next
		senglish:		
			call writem
			add di, 4
			call writep
			add di, 4
			call writeh
			add di, 4
			call getSpeedMi
		jmp next
	max:					;max speed [mode]
		call writem
		add di, 4
		call writea
		add di, 4
		call writex
		add di, (80 * 35) - 8
		cmp [M_E], 0
		je mmetric
		jmp menglish
		mmetric:
			call writek
			add di, 4
			call writep
			add di, 4
			call writeh	
			add di, 4
			mov ax, [maxSpeed]
		jmp next
		menglish:
			call writem
			add di, 4
			call writep
			add di, 4
			call writeh
			add di, 4
			call getMaxSpeedMi
		jmp next
	
	next:
		add di, 4
		call writeax
	popad
	ret
writemode endp
;////////////////////////////////////////////////////
; Description: Writes number in AX to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;////////////////////////////////////////////////////
WriteAX proc
	pushad
	mov cx, 0
	mov dx, 0
	mov si, 1000
	div si
	cmp ax, 0
	jz skip1
	inc cx
	call subwriteAX
	jmp noskip1
skip1:
	call writespace
noskip1:
	add di, 4
	mov ax, dx
	mov dx, 0
	mov si, 100
	div si
	cmp cx, 0
	jne nocomparison1
	cmp ax, 0
	jz skip2
	inc cx
nocomparison1:
	call subwriteAX
	jmp noskip2
skip2:
	call writespace
noskip2:
	add di, 4
	mov ax, dx
	mov dx, 0
	mov si, 10
	div si
	cmp cx, 0
	jne nocomparison2
	cmp ax, 0
	jz skip3
	inc cx
nocomparison2:
	call subwriteAX
	jmp noskip3
skip3:
	call writespace
noskip3:
	add di, 4
	call writedot
	add di, 4
	mov ax, dx
	call subwriteAX
	popad
	ret
writeAX endp

; Called by writeAX, not intended for user calls
subwriteAX proc
	cmp ax, 0
	jne notzero
	call writezero
	jmp escape
notzero:
	cmp ax, 1
	jne notone
	call writeone
	jmp escape
notone:
	cmp ax, 2
	jne nottwo
	call writetwo
	jmp escape
nottwo:
	cmp ax, 3
	jne notthree
	call writethree
	jmp escape
notthree:
	cmp ax, 4
	jne notfour
	call writefour
	jmp escape
notfour:
	cmp ax, 5
	jne notfive
	call writefive
	jmp escape
notfive:
	cmp ax, 6
	jne notsix
	call writesix
	jmp escape
notsix:
	cmp ax, 7
	jne notseven
	call writeseven
	jmp escape
notseven:
	cmp ax, 8
	jne noteight
	call writeeight
	jmp escape
noteight:
	call writenine	
escape:
	ret
subwriteAX endp
;===================================================
;*****************SPIN WHEEL CODE********************
;===================================================
wheelrotate1 proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 00000000111111000011111100000000b
	add di, 80
	loop top
	mov cx, 5
toptopmid:
	mov dword ptr es:[di], 11000000110000111100001100000011b
	add di, 80
	loop toptopmid
	mov cx, 6
topbottommid:
	mov dword ptr es:[di], 00111100110000000000001100111100b
	add di, 80
	loop topbottommid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111111111111111111111111111111b
	add DI, 80	
	loop mid
	mov cx, 6
bottomtopmid:
	mov dword ptr es:[di], 00111100110000000000001100111100b
	add di, 80
	loop bottomtopmid
	mov cx, 5
bottombottommid:
	mov dword ptr es:[di], 11000000110000111100001100000011b
	add di, 80
	loop bottombottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 00000000111111000011111100000000b
	add di, 80
	loop bottom
	popad
	ret
wheelrotate1 endp
;===================================================
wheelrotate2 proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 00000000111111000011111100000000b
	add di, 80
	loop top
	mov cx, 5
toptopmid:
	mov dword ptr es:[di], 11000000000000111100000000000011b
	add di, 80
	loop toptopmid
	mov cx, 6
topbottommid:
	mov dword ptr es:[di], 00111100001111000011110000111100b
	add di, 80
	loop topbottommid
	mov cx, 3
mid:
	mov dword ptr es:[di], 00001111111000000000011111110000b
	add DI, 80	
	loop mid
	mov cx, 6
bottomtopmid:
	mov dword ptr es:[di], 00111100001111000011110000111100b
	add di, 80
	loop bottomtopmid
	mov cx, 5
bottombottommid:
	mov dword ptr es:[di], 11000000000000111100000000000011b
	add di, 80
	loop bottombottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 00000000111111000011111100000000b
	add di, 80
	loop bottom
	popad
	ret
wheelrotate2 endp
;===================================================
;*******************END SPIN WHEEL*******************
;===================================================
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeupdown proc
	pushad
	mov cx, 4
toptop:
	mov dword ptr es:[di], 00000000111000000000011100000000b	
	add di, 80	
	loop toptop
	mov cx, 3
top:
	mov dword ptr es:[di], 00000000111110000001111100000000b	
	add di, 80	
	loop top
	mov cx, 3
midtoptop:
	mov dword ptr es:[di], 11000000111111111111111100000011b	
	add di, 80	
	loop midtoptop
	mov cx, 4
midtop:
	mov dword ptr es:[di], 11111110111111111111111101111111b	
	add di, 80	
	loop midtop
	mov cx, 5
mid:
	mov dword ptr es:[di], 00000000000000000000000000000000b	
	add di, 80		
	loop mid
	mov cx, 4
midbottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b	
	add di, 80	
	loop midbottom
	mov cx, 3
midbottombottom:
	mov dword ptr es:[di], 11000000111111111111111100000011b
	add di, 80	
	loop midbottombottom
	mov cx, 3
bottom:
	mov dword ptr es:[di], 00000000111110000001111100000000b	
	add di, 80	
	loop bottom
	mov cx, 4
bottombottom:	
	mov dword ptr es:[di], 00000000111000000000011100000000b
	add di, 80	
	loop bottombottom
	popad 
	ret
writeupdown endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writea proc
	pushad
	mov cx, 5
top:
	mov dword ptr es:[di], 00000000111110000001111100000000b
	add Di, 80		
	loop top
	mov cx, 8
topmid:	
	mov dword ptr es:[di], 00000000000111111111100000000000b
	add Di, 80		
	loop topmid
	mov cx, 5
mid:	
	mov dword ptr es:[di], 11000000111111111111111100000011b
	add Di, 80		
	loop mid
	mov cx, 13
bottom:	
	mov dword ptr es:[di], 11111000000000011000000000011111b
	add Di, 80		
	loop bottom
	popad
	ret
writea endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writex proc
	pushad
	mov cx, 10
tophalf:
	mov dword ptr es:[di], 11111110000001111110000001111111b
	add Di, 80		
	loop tophalf
	mov cx, 13
mid:
	mov dword ptr es:[di], 00000000111110000001111100000000b
	add DI, 80		
	loop mid
	mov cx, 10
bottomhalf:
	mov dword ptr es:[di], 11111110000001111110000001111111b
	add Di, 80	
	loop bottomhalf
	popad
	ret
writex endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeh proc
	pushad
	mov cx, 14
tophalf:
	mov dword ptr es:[di], 00000000000000000000000001111110b
	add Di, 80		
	loop tophalf
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 15
bottomhalf:
	mov dword ptr es:[di], 00111110000000000000000001111100b
	add Di, 80	
	loop bottomhalf
	popad
	ret
writeh endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writee proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80	
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 11
bottommid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80
	loop bottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writee endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writen proc
	pushad
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop mid
	mov cx, 29
bottom:
	mov dword ptr es:[di], 00111110000000000000000001111100b
	add di, 80
	loop bottom
	popad
	ret
writen endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writef proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80	
	loop topmid
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 14
bottommid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80
	loop bottommid
	popad
	ret
writef endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeu proc
	pushad
	mov cx, 29
mid:
	mov dword ptr es:[di], 00111110000000000000000001111100b
	add di, 80
	loop mid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop bottom
	popad
	ret
writeu endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writer proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 12
topmid:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80
	loop topmid
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 6
bottomhalftop:
	mov dword ptr es:[di], 00000000111100001111111101111111b
	add Di, 80		
	loop bottomhalftop
	mov cx, 7
bottomhalfbottom:
	mov dword ptr es:[di], 11111110000011110000000001111111b
	add Di, 80		
	loop bottomhalfbottom
	popad
	ret
writer endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writec proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop top
	mov cx, 25
mid:
	mov dword ptr es:[di], 00000000000000000000000001111100b
	add di, 80
	loop mid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop bottom
	popad
	ret
writec endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writep proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 12
topmid:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80
	loop topmid
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 13
bottomhalf:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80		
	loop bottomhalf
	popad
	ret
writep endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writed proc
	pushad 
	mov cx, 13
tophalf:
	mov dword ptr es:[di], 01111110000000000000000000000000b
	add Di, 80		
	loop tophalf
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 12
bottomhalf:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80	
	loop bottomhalf
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop bottom
	popad
	ret
writed endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writes proc
	pushad
	mov cx, 5
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 9
topmid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80
	loop topmid
	mov cx, 5
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 9
bottommid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80
	loop bottommid
	mov cx, 5
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writes endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writedot proc
	pushad
	mov cx, 24
top:
	mov dword ptr es:[di], 00000000000000000000000000000000b
	add Di, 80
	loop top
	mov cx, 9
dot:
	mov dword ptr es:[di], 00000000111110000001111100000000b	
	add di, 80
	loop dot
	popad
	ret
writedot endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writei proc
	pushad
	mov cx, 7
dot:
	mov dword ptr es:[di], 00000000111110000001111100000000b	
	add di, 80	
	loop dot
	mov cx, 5
mid:
	mov dword ptr es:[di], 00000000000000000000000000000000b	
	add di, 80	
	loop mid
	mov cx, 21
body:
	mov dword ptr es:[di], 00000000111110000001111100000000b	
	add di, 80	
	loop body
	popad
	ret
writei endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writek proc
	pushad
	mov cx, 7 
top:
	mov dword ptr es:[di], 11111110000000000000000001111111b	
	add DI, 80
	loop top
	mov cx, 7
topmid:
	mov dword ptr es:[di], 00000000011111110000000001111111b
	add di, 80
	loop topmid
	mov cx, 5
mid:
	mov dword ptr es:[di], 00000000100000001111111101111111b
	add DI, 80
	loop mid
	mov cx, 7
bottommid:
	mov dword ptr es:[di], 00000000011111110000000001111111b
	add di, 80
	loop bottommid
	mov cx, 7 
bottom:
	mov dword ptr es:[di], 11111110000000000000000001111111b	
	add DI, 80
	loop bottom
	popad
	ret
writek endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writem proc
	pushad
	mov cx, 8
top:
	mov dword ptr es:[di], 10000000000111111111100000000001b
	add DI, 80
	loop top
	mov cx, 25
bottom:
	mov dword ptr es:[di], 01111111111000000000011101111110b
	add DI, 80
	loop bottom
	popad
	ret
writem endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writenine proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 11
bottomhalf:
	mov dword ptr es:[di], 01111110000000000000000000000000b
	add Di, 80		
	loop bottomhalf
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writenine endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeeight proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 11
bottomhalf:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80		
	loop bottomhalf
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writeeight endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeseven proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 29
mid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80
	loop mid
	popad
	ret
writeseven endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writesix proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 00000000000000000000000001111110b
	add Di, 80
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 11
bottomhalf:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80		
	loop bottomhalf
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writesix endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writefive proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 11
bottommid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80
	loop bottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writefive endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writefour proc
	pushad
	mov cx, 14
tophalf:
	mov dword ptr es:[di], 01111110000000000000000001111110b
	add Di, 80		
	loop tophalf
	mov cx, 4
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 15
bottomhalf:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80	
	loop bottomhalf
	popad
	ret
writefour endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writethree proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80	
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80		
	loop mid
	mov cx, 11
bottommid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80
	loop bottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writethree endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writetwo proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop top
	mov cx, 11
topmid:
	mov dword ptr es:[di], 11111110000000000000000000000000b
	add Di, 80
	loop topmid
	mov cx, 3
mid:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop mid
	mov cx, 11
bottommid:
	mov dword ptr es:[di], 00000000000000000000000001111111b
	add Di, 80
	loop bottommid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add DI, 80	
	loop bottom
	popad
	ret
writetwo endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writeone proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 00000000111100001111111101111111b
	add DI, 80
	loop top
	mov cx, 25
mid:
	mov dword ptr es:[di], 00000000111100000000111100000000b
	add di, 80
	loop mid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop bottom
	popad
	ret
writeone endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writezero proc
	pushad
	mov cx, 4
top:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop top
	mov cx, 25
mid:
	mov dword ptr es:[di], 00111110000000000000000001111100b
	add di, 80
	loop mid
	mov cx, 4
bottom:
	mov dword ptr es:[di], 11111110111111111111111101111111b
	add di, 80
	loop bottom
	popad
	ret
writezero endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
writespace proc
	pushad
	mov cx, 33
mid:
	mov dword ptr es:[di], 00000000000000000000000000000000b
	add di, 80
	loop mid	
	popad
	ret
writespace endp
;/////////////////////////////////////////////////////////
; Description: Writes character to graphics memory
; Receives: screen position in Di
; Returns: N/A
; Requires: N/A
; Clobbers: FLAGS
;/////////////////////////////////////////////////////////
clearscreen proc
	pushad
	mov ax, 0
	mov di, 0
	mov cx, 19200
	cld
	rep stosw
	popad
	ret
clearscreen endp
;===================================
END main
