
		LIST 	P=PIC16F877
		include	<P16f877.inc>
 __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_OFF & _HS_OSC & _WRT_ENABLE_ON & _LVP_OFF & _DEBUG_OFF & _CPD_OFF
;---------------------------------------------------------------------------------------;

		org		0x00
reset:	goto	start
		org		0x04
		goto	psika
		org		0x10
start:	bcf		STATUS, RP0
		bcf		STATUS, RP1			;Bank0 <------

;-------------------------Initilaize Area----------------------------------------------;
		
		clrf    0x57  	 ; clear the first digit register
    	clrf    0x58   	; clear the second digit register
    	clrf    0x59  	; clear the third digit register
		clrf    0x70	; counter
		clrf    0x71 	;flag status plus=1 minus=2 no=0	
		clrf 	0x72	;flag 10 timer
		clrf 	0x73	;volt
		clrf	0x79	;flag 1 timer
	
		clrf  	PORTA
		clrf	PORTD
		clrf	PORTE

		clrf	INTCON		;Disable all interrupts
		clrf	PIR1

		bsf		STATUS, RP0		
;-------------------------------------Bank1 <-------------------------------------------------;
		
		clrf	TRISD
		clrf	PIE1
		bsf		PIE1, TMR1IE		; TIMER1 interrupt Enable
		bsf		PIE1, ADIE			;Enable ADC Interrupt
		movlw	0x02
		movwf	ADCON1			   	; all A analog;all E digital
									; format : 6 lower bit of ADRESL =0
		movlw	0xff
		movwf	TRISA				;porta input
		clrf	TRISE
		bcf		STATUS,RP0		;Bank0 <-
		
		call 	initT
		call 	initLcd

		movlw	0x81				;B'10000001'
		movwf	ADCON0				;Fosc/32, channel_0, ADC on
		;call	d_20				;Delay TACQ
	

		bsf 	INTCON, PEIE		;Enable Peripherals interrupts
		bsf		INTCON, GIE			;Enable Interrupts

;------------------------------------MAIN--------------------------------------------;

main:	call check10
		btfss 0x79, 0
		goto main
		bsf		ADCON0, GO			;Start conversion
		call d_20		
		call checkV	; check the volt
		btfsc 0x71,0
		call Up
		btfsc 0x71, 1
		call Down
		btfsc 0x71,0
		call lcdup
		btfsc 0x71, 1
		call lcddown
		btfsc 0x71, 0
		goto lolo
		btfsc 0x71, 1
		goto lolo
		call lcdStop
lolo:	call separate
		call lcd
		clrf 0x79
		clrf 0x72
		goto main


;---------- Interrupt program: ---------------------------------------------------------;

psika: 	movwf	0x7A				;store W_reg --> 0x7A
		swapf	STATUS, w
		movwf	0x7B				;store STATUS --> 0x7B

		bcf  	STATUS, RP0
		bcf		STATUS, RP1  		;Bank0 <------


		btfsc	PIR1, TMR1IF		;check timer1 int flag
		goto	timerP
		btfsc 	PIR1, ADIF    ;check psika analog digital 
		goto	AtD
ERR:	goto	ERR

AtD:	bcf 	PIR1, ADIF			;Clear AD Flag
		movf 	ADRESH, w
		movwf 0x73
		;movwf PORTD
		;call	d_4
		;call 	sdel
		;call 	mdel
		;bsf 	ADCON0, GO			;Start new conversion
		;call d_20
		swapf	0x7B, w
		movwf	STATUS				;restore STATUS <-- 0x7B
		swapf	0x7A, f
		swapf	0x7A, w				;restore W_reg <-- 0x7A
		retfie		

timerP:	bcf		T1CON, TMR1ON		;stop timer1
		incf 	0x72, 1  ;inc flag 10 timer

		
		
con:	clrf	TMR1L
		clrf	TMR1H
		bcf		PIR1, TMR1IF

		swapf	0x7B, w
		movwf	STATUS				;restore STATUS <-- 0x7B
		swapf	0x7A, f
		swapf	0x7A, w				;restore W_reg <-- 0x7A
		bsf		T1CON, TMR1ON		;start timer1
		retfie

;------------------------Function Area----------------------------------------------------;

checkV	movf 0x73, w
		movwf PORTD
		bcf STATUS, C
		clrw
		movlw d'26'
		subwf 0x73, 1
		btfss STATUS, C
		goto no ;;if between do nothing x<26
		clrw
		movlw d'51'
		bcf STATUS, C
		subwf 0x73, 1
		btfss STATUS, C
		goto upL	;; 26<x<77
		clrw
		movlw d'16'
		bcf STATUS, C
		subwf 0x73, 1
		btfss STATUS, C
		goto no ; 77<x<93
		clrw
		movlw d'25'
		bcf STATUS, C
		subwf 0x73, 1
		btfss STATUS, C
		goto downL ; 93<x<118
		goto no
f:		return

no:		movlw 0x00
		movwf 0x71
		goto f

upL:	movlw 0x01
		movwf 0x71
		goto f

downL:	movlw 0x02
		movwf 0x71
		goto f


Down	bcf STATUS, C
		decf 0x70, 1
		btfss 0x70, 0
		goto l
		btfss 0x70, 1
		goto l
		btfss 0x70, 2
		goto l
		btfss 0x70, 3
		goto l
		btfss 0x70, 4
		goto l
		btfss 0x70, 5
		goto l
		btfss 0x70, 6
		goto l
		btfss 0x70, 7
		goto l
		movlw d'250'
		movwf 0x70
l:		return

Up		clrw
		bcf	STATUS, C
		movlw d'250'
		subwf 0x70, 0
		btfsc STATUS, C ; if we pass 250
		goto ni
		goto inc
ab:		return

inc:	incf 0x70, 1 ; counter++
		goto ab

ni:		clrf 0x70 ;counter=0
		goto ab

check10 btfsc 0x72, 0  ;;if not pass 1 sec loop
		goto	h 
		btfss 0x72, 1  
		goto	h
		btfsc 0x72, 2  
		goto	h
		btfss 0x72, 3  
		goto	h 
		incf 0x79, 1 ;flag of 10 times
h:		return

separate ; separate the digits
    	clrf    0x57   ; clear the first digit register
    	clrf    0x58   ; clear the second digit register
    	clrf    0x59  ; clear the third digit register	
		movf	0x70, w
		movwf	0x50
		bcf	STATUS, C
lul100:	movlw d'100'
		subwf 0X50, 1 
		btfss STATUS, C
		goto lul10
		incf 0x59, 1
		goto lul100
		
lul10:  addwf 0x50, 1
lul10a:	movlw d'10'
		subwf 0X50, 1 
		btfss STATUS, C
		goto lul1
		incf 0x58, 1
		goto lul10a
lul1:  	addwf 0x50, 1
lul1a:	movlw d'1'
		subwf 0X50, 1 
		btfss STATUS, C
		goto g
		incf 0x57, 1
		goto lul1a
g:		return


;lcd	
	;	bcf		STATUS, RP0
	;	bcf		STATUS, RP1		;Bank 0
	;	clrf	PORTD
	;	clrf	PORTE
;
;		bsf		STATUS, RP0		;Bank 1
;		movlw	0x06			;Digital
;		movwf	ADCON1
;
;		clrf	TRISE		;porte output 
;		clrf	TRISD		;portd output
;
;		bcf		STATUS, RP0		;Bank 0

	;	call initLcd
clear:	movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel
		return	
;----------------------------*------------------------------	
lcdStop:movlw B'11000000' ;PLACE for the data on the LCD
		movwf 0x20 ;B'11000100'
		call lcdc
		call mdel

		movlw 0x53 ;S CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x54 ;T CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x4f ;O CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x50 ;P CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		return

lcddown:movlw B'11000000' ;PLACE for the data on the LCD
		movwf 0x20 ;B'11000100'
		call lcdc
		call mdel

		movlw 0x44 ;D CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x4f ;O CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x57 ;W CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x4E ;N CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		return


lcdup:	movlw B'11000000' ;PLACE for the data on the LCD
		movwf 0x20 ;B'10000100'
		call lcdc
		call mdel

		movlw 0x55 ;U CHAR (the data )
		movwf 0x20
		call lcdd
		call mdel

		movlw 0x50 ;P CHAR (the data )
		movwf 0x20
		call lcdd
		call	mdel
		
		movlw 0x14 ;clear
		movwf 0x20
		call lcdd
		call	mdel

		movlw 0x14 ;clear
		movwf 0x20
		call lcdd
		call	mdel
		return


lcd		movlw	B'10000000' 			 ;PLACE for the data on the LCD
		movwf	0x20			;B'10000000'
		call 	lcdc
		call	mdel
		
		movlw 0x30 	;convert the counter to char
		addwf 0x59, 1
		movlw 0x30
		addwf 0x58, 1
		movlw 0x30
		addwf 0x57, 1
		
		movf 	0x59 ,0		;hundred CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel

		movf 	0x58 ,0		;tens CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel


		movf 	0x57 ,0		;ones CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
	
		return

;subroutine to initialize LCD
;
initLcd:movlw	0x30 ;00011110
		movwf	0x20
		call 	lcdc
		call	del_41

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	del_01

		movlw	0x30
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x01		; display clear
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x06		;3. ID=1,S=0 increment,no  shift 000001 ID S  
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x0c		;4. D=1,C=B=0 set display ,no cursor, no blinking
		movwf	0x20
		call 	lcdc
		call	mdel

		movlw	0x38		; dl=1 ( 8 bits interface,n=2 lines,f=5x8 dots)
		movwf	0x20
		call 	lcdc
		call	mdel
		return

;
;subroutine to write command to LCD
;

lcdc:	movlw	0x00		; E=0,RS=0 
		movwf	PORTE
		movf	0x20,w
		movwf	PORTD
		movlw	0x01		; E=1,RS=0
		movwf	PORTE
        call	sdel
		movlw	0x00		; E=0,RS=0
		movwf	PORTE
		return

;
;subroutine to write data to LCD
;

lcdd:	movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
		return

d_20:	movlw	0x20
		movwf	0x22
lulaa11:decfsz	0x22, f
		goto	lulaa11
		return

d_4		movlw	0x06
		movwf	0x22
lulaa22:decfsz	0x22, f
		goto	lulaa22
		return

;delay lcd

del_41:	movlw		0xcd
		movwf		0x23
lulaa6:	movlw		0x20
		movwf		0x22
lulaa7:	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return


del_01:	movlw		0x20
		movwf		0x22
lulaa8:	decfsz		0x22,1
		goto		lulaa8
		return


sdel:	movlw		0x19		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2:	movlw		0xfa
		movwf		0x22
lulaa1:	decfsz		0x22,1		; decfsz= 12 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return


mdel:	movlw		0x0a
		movwf		0x24
lulaa5:	movlw		0x19
		movwf		0x23
lulaa4:	movlw		0xfa
		movwf		0x22
lulaa3:	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return

initT	movlw	0x30                ;00110000
		movwf	T1CON				; internal clock source with 1:8 prescaler
		clrf	TMR1L
		clrf	TMR1H				;TMR1H:TMR1L = 0x0AF0 = 2800d
	;	Td = 200ns*(2^16-TMR1H:TMR1L)PS = 200ns(2^16-0)*8 ~= 0.104s
		clrf	PIR1				;clear peripheral interrupt flags

;****** TIMER 1 on
		bsf		T1CON, TMR1ON		; Timer 1 starts to increment
		return


delay_500m:					;-----> 500ms delay
		movlw		0x32			;N1 = 50d
		movwf		0x51
CONT5:	movlw		0x80			;N2 = 128d
		movwf		0x52
CONT6:	movlw		0x80			;N3 = 128d
		movwf		0x53
CONT7:	decfsz		0x53, f
		goto		CONT7
		decfsz		0x52, f
		goto		CONT6
		decfsz		0x51, f
		goto		CONT5
		return						; D = (5+4N1+4N1N2+3N1N2N3)*200nsec = (5+4*50+4*50*128+3*50*128*128)*200ns = 496.7ms=~500ms

		end