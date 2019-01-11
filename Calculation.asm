INCLUDE Irvine32.inc

KEY = 239     	; any value between 1-255
BUFMAX = 128     	; maximum buffer size

.data
sPrompt1 BYTE  "Use these following char for shape",0
sShape	 BYTE  "s:Square	t:Triangle	 r:Rectangle	q:Quit",0
sPrompt2 BYTE  "Enter the char: ",0
sSquare	 BYTE  "::: Begin calculate square area :::",0
sRec	 BYTE  "::: Begin calculate rectangle area :::",0
sCircle  BYTE  "::: Begin calculate circle area :::",0
sTri	 BYTE  "::: Begin calculate triangle area :::",0

swidth	 BYTE  "Enter width:	",0
slength	 BYTE  "Enter length:	",0
sHeight	 BYTE  "Enter height:	",0
sBase	 BYTE  "Enter base:	",0
sArea	 BYTE  "The area is:	",0
buffer   BYTE   BUFMAX+1 DUP(0)
bufSize  DWORD  ?
ans		 DWORD  ?
.code
main PROC
	call InputTheString		; input the plain texts
	mov  esi,0		; index 0 in buffer
	mov  al,buffer[esi]
	cmp al, 'q'		; square
	je quitProg
	call Determine
	call DisplayArea
quitProg:
	exit
main ENDP


InputTheString PROC
	pushad
	mov  edx,OFFSET sPrompt1		
	call WriteString
	call Crlf
	mov  edx,OFFSET sShape		
	call WriteString
	call Crlf
	mov  edx,OFFSET sPrompt2		
	call WriteString
	mov  ecx,BUFMAX         		; maximum character count
	mov  edx,offset buffer   		; point to the buffer
	call ReadString         		; input the string
	mov  bufSize,eax        		; save the length
	popad
	ret
InputTheString ENDP

DisplayArea PROC
	mov  edx,OFFSET sArea
	call WriteString
	mov  edx,ans
	call writeDec
	call Crlf
	call Crlf
	ret
DisplayArea ENDP

Determine PROC
	mov  esi,0		; index 0 in buffer
	mov  al,buffer[esi]
	cmp al, 's'		; square
	je square
	cmp al, 't'		; triangle
	je triangle
	cmp al, 'r'		; rectangle
	je rectangle
	cmp al, 'q'		; rectangle
	je endDet
square:
	call SquareCal
	jmp endDet
triangle:
	call TriangleCal
	jmp endDet
rectangle:
	call RecCal
	jmp endDet
endDet:
	ret
Determine ENDP

SquareCal PROC
	mov  edx,OFFSET sSquare
	call WriteString
	call Crlf
	mov  edx,OFFSET swidth
	call WriteString
	call ReadDec
	mov ecx, eax
	mul ecx
	mov ans, eax
	ret
SquareCal ENDP

TriangleCal PROC
	mov  edx,OFFSET sTri
	call WriteString
	call Crlf
	mov  edx,OFFSET sBase
	call WriteString
	call ReadDec
	mov ebx, eax			;ebx store base
	mov  edx,OFFSET sHeight
	call WriteString
	call ReadDec			;eax store height
	mul ebx
	mov ecx, 2
	div ecx
	mov ans, eax
	ret
TriangleCal ENDP

RecCal PROC
	mov  edx,OFFSET sRec
	call WriteString
	call Crlf
	mov  edx,OFFSET swidth
	call WriteString
	call ReadDec
	mov ebx, eax			;ebx store base
	mov  edx,OFFSET sLength
	call WriteString
	call ReadDec			;eax store height
	mul ebx
	mov ans, eax
	ret
RecCal ENDP

END main