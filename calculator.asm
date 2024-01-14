
;**** Calculator programming **** 

ORG 00H
	
	KEYPAD EQU P3
	RO1 BIT P3.7
	RO2 BIT P3.6
	RO3 BIT P3.5
	RO4 BIT P3.4
	C1 BIT P3.0
	C2 BIT P3.1
	C3 BIT P3.2
	C4 BIT P3.3
		
	LCD_PORT EQU P1
	RS BIT P2.0
	E BIT P2.1
	
    MOV KEYPAD , #0FH	
	MOV LCD_PORT , #00H
	CLR RS
	CLR E
	LCALL LCD_INIT
	
START: 
	;=====INPUT_1======;			
	           ACALL READ_KEYPAD
	           ACALL CHICK_INPUT
	           CJNE A ,#0 , Operand1_Digit1
	           LCALL ERROR_MSG
	           SJMP START
			   ;===============================OPERAND_1------> R2 =======================================;
Operand1_Digit1:
	           MOV A ,R1
	           ACALL LCD_DATA
	           ACALL DELAY
	           ANL A,#0FH
	           MOV R2,A
	;======INPUT_2=====;
	INPUT_2:    ACALL READ_KEYPAD
			   CJNE A,#'C', CONTINUE1
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   SJMP START 
			   CONTINUE1:  ACALL CHICK_OPR
	           CJNE A ,#0 , Operand1_Digit2
	           SJMP STORE_OPERATOR
Operand1_Digit2: 
	           MOV A ,R1
	           ACALL LCD_DATA
	           ACALL DELAY
	           ANL A,#0FH
Operand1_2_Digits:			   
	           MOV R1 ,A
	           MOV A ,R2
	           MOV B,#10
	           MUL AB
	           ADD A,R1
	           MOV R2 ,A
	;======INPUT_3=====;
	INPUT_3:    ACALL READ_KEYPAD 
			   CJNE A,#'C', CONTINUE2
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,R2
			   MOV B,#10
	           DIV AB
			   MOV R2,A
			   MOV B,#0
			   SJMP  INPUT_2
			   
			   CONTINUE2:
			   ACALL CHICK_OPR
			   CJNE A , #0 , Operand1_OVERFLOW       
			   SJMP STORE_OPERATOR
Operand1_OVERFLOW:
	           LCALL OVERFLOW_MSG
	           SJMP START
    ;===============================OPERATOR ---->>>> R3 =======================================;
STORE_OPERATOR:
	           MOV A ,R1
	           MOV R3,A
			   ACALL LCD_DATA
			   ACALL DELAY
	;===============================OPERAND_2---->>>> R4 =======================================;
	INPUT4:	   ACALL READ_KEYPAD
              CJNE A,#'C', CONTINUE3
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   SJMP INPUT_3
			   
			   CONTINUE3:
			   CJNE A,#'+',compet
			   MOV A,#'+'
			   ACALL LCD_DATA
			   ACALL DELAY
			   LJMP advanced_op
			   compet:  
			   ACALL CHICK_INPUT
			   CJNE A ,#0 , Operand2_Digit1
	           LCALL ERROR_MSG
	           LJMP START
Operand2_Digit1:
	           MOV A ,R1
	           ACALL LCD_DATA
	           ACALL DELAY
	           ANL A,#0FH
	           MOV R4,A
			  
			  INPUT5:
			  LCALL READ_KEYPAD
			   
			   CJNE A,#'C', CONTINUE4
			   MOV A,#10H
			   ACALL LCD_COMM
			  MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   SJMP INPUT4
			   
			   CONTINUE4:			   
			   CJNE A,#'=',NOT_EQUAL1
			   LJMP CALC_RESULT
			   
NOT_EQUAL1:	 
               ACALL CHICK_INPUT
	           CJNE A ,#0 ,Operand2_Digit2
			   LCALL ERROR_MSG
			   LJMP START
Operand2_Digit2: 
               MOV A ,R1
			   ACALL LCD_DATA
			   ACALL DELAY
			   ANL A  ,#0FH
			   MOV R1 ,A
Operand2_2_Digits:			   
			   MOV A ,R4
			   MOV B,#10
			   MUL AB
			   ADD A,R1
			   MOV R4 ,A
			   
			   ACALL READ_KEYPAD
			   CJNE A,#'C', CONTINUE5
			   MOV A,#10H
			   ACALL LCD_COMM
			  MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,R4
			   MOV B,#10
	           DIV AB
			   MOV R4,A
			   MOV B,#0
			   SJMP INPUT5
			   
CONTINUE5:		
			   CJNE A,#'=',NOT_EQUAL2
			   LJMP CALC_RESULT
NOT_EQUAL2:	   ACALL CHICK_INPUT
	           CJNE A ,#0 ,Operand2_OVERFLOW
			   LCALL ERROR_MSG
			   LJMP START
Operand2_OVERFLOW:
	           LCALL OVERFLOW_MSG
	           LJMP START			   
			   
			   
			   
CALC_RESULT:			   
             	MOV A ,#'='
	            ACALL LCD_DATA
	            ACALL DELAY

DIVISION:	    CJNE R3 ,#'/' , NOT_DIVISION
	            MOV A ,R4
				JZ DIV_BY_ZERO
				MOV B,R4
				MOV A,R2
				DIV AB
				MOV R7,B
				ACALL LCD_NUM
				
				MOV B,R7
	            MOV A,B
				JZ KEEP_KEEP
				MOV A ,#'.'
				ACALL LCD_DATA
				ACALL DELAY
DIGIT_1_AFTER_DECIMAL:
				MOV A,#10
				MUL AB
				MOV B,R4
	            DIV AB
				MOV R7,B
				ACALL LCD_NUM
				
				MOV B,R7
	            MOV A,B
				JZ KEEP_KEEP
DIGIT_2_AFTER_DECIMAL:
				MOV A,#10
				MUL AB
				MOV B,R4
				DIV AB
				MOV R7,B
	            ACALL LCD_NUM
			    MOV B,R7
	            MOV A,B
	            JZ KEEP_KEEP
DIGIT_3_AFTER_DECIMAL:
	            MOV A,#10
	            MUL AB
	            MOV B,R4
	            DIV AB
	            ACALL LCD_NUM
	            LJMP KEEP
	
DIV_BY_ZERO:
	            ACALL ERROR_MSG
	            LJMP START


KEEP_KEEP: 
	   ACALL READ_KEYPAD
	   CJNE A,#'C' , KEEP_KEEP
	   ACALL LCD_CLEAR
	   LJMP START	
	
			
NOT_DIVISION:
                CJNE R3 ,#'*' , NOT_MULTIPLIER 
				MOV A , R4
				MOV B , R2
				MUL AB
				MOV R0,A
				MOV A,B
				JNZ GREATER_THAN_255
				MOV A,R0
				LCALL LCD_NUM
				LCALL KEEP_KEEP

GREATER_THAN_255:
                MOV A,B
	      ;======(HIGH_BYTE)======;			
				CJNE A,#03H,NEXT  
				MOV A,R0  
		 ;======(LOW_BYTE)======;
				CJNE A,#0E7H,NEXT
				SJMP EQUAL_999
NEXT:
                JNC GREATER_THAN_999
	            SJMP LESS_THAN_999
 
GREATER_THAN_999: 
                MOV R5,#00H
 SUB_1000:      MOV A,R0
	            CLR C
	            SUBB A,#0E8H  
				MOV R0,A      
				MOV A,B       
				SUBB A,#03H   
				MOV B,A       
				INC R5
	      ;======(HIGH_BYTE)======;			
				CJNE A,#03H,NEXT1_1  
				MOV A,R0  
		 ;======(LOW_BYTE)======;
				CJNE A,#0E7H,NEXT1_1
				SJMP DIGIT_1
NEXT1_1:
                JC DIGIT_1
	            SJMP SUB_1000
			   
	
	;==================================================================;
DIGIT_1:       MOV A,R0
			   MOV A,R5
			   LCALL LCD_NUM
			   LCALL DELAY
LESS_THAN_999:
EQUAL_999:
			   MOV R6,#00H 
      
               MOV A,R0
               MOV A,B
      ;======(HIGH_BYTE)======;
SUB_100:	   CJNE A,#00H,NEXT2_2
	 ;======(LOW_BYTE)======;
			   MOV A,R0  
			   CJNE A,#63H,NEXT2_2
			   SJMP DIGIT_2
NEXT2_2:
               JC DIGIT_2
	           
               MOV A,R0
			   CLR C
			   SUBB A,#64H  
			   MOV R0,A      
			   MOV A,B       
			   SUBB A,#00H   
			   MOV B,A       
			   INC R6
               SJMP SUB_100
			   

DIGIT_2:
			   MOV A,R0
			   MOV A,R6
			   LCALL LCD_NUM
			   LCALL DELAY
    

               MOV A,R0
			   MOV B,#10
			   DIV AB
			   ACALL LCD_NUM
			   LCALL DELAY
			   MOV A,B
			   ACALL LCD_NUM
			   LCALL DELAY
			   			   
LJMP KEEP_KEEP



	
NOT_MULTIPLIER:
	            CJNE R3 ,#'+' , NOT_ADDER
				MOV A , R2      
				ADD A , R4      
				ACALL LCD_NUM
				LJMP KEEP
	
NOT_ADDER:      CJNE R3 ,#'-' ,KEEP
                MOV A,R2
				CLR C
				SUBB A , R4
				JC NEGATIVE
				ACALL LCD_NUM
				LJMP KEEP
				
NEGATIVE:
	            CPL A
				ADD A,#1
				MOV R0,A
				MOV A ,#'-'
				ACALL LCD_DATA
				ACALL DELAY
				MOV A,R0
				ACALL LCD_NUM
				SJMP KEEP
	
	
KEEP: 
	   ACALL READ_KEYPAD
	   CJNE A,#'C' , KEEP
	   ACALL LCD_CLEAR
	   LJMP START
	   ;************************ADVANCED OPERATIONS *******************   
	   advanced_op:
	          ACALL READ_KEYPAD
			  CJNE A,#'C',STAYHERE 
			   MOV A,#10H
			   ACALL LCD_COMM
			   MOV A,#' ' 
			   ACALL DELAY
			   ACALL DELAY
			   ACALL LCD_DATA
			   ACALL DELAY
			   ACALL DELAY
			   MOV A,#10H
			   ACALL LCD_COMM
			   LJMP INPUT4
	   STAYHERE:CJNE A,#'=',STAYHERE
	            MOV A ,#'='
	            ACALL LCD_DATA
	            ACALL DELAY
				
				INVERSE: CJNE R3 ,#'-' , FACTORIAL
				MOV B,R2
				MOV A,#1
				DIV AB
				ACALL LCD_NUM
				MOV A,R2
				MOV R4,A
				MOV A ,#'.'
				ACALL LCD_DATA
				ACALL DELAY
                LJMP DIGIT_1_AFTER_DECIMAL
				
			     FACTORIAL:CJNE R3 ,#'+' , POWER2
				 MOV A,R2
				 FACT:
				 DEC R2
				 CJNE R2,#1,REPET
				 SJMP STOP
				 REPET:
				 MOV B,R2
				 MUL AB
				 SJMP FACT
				 STOP :
				 ACALL LCD_NUM
                 LCALL KEEP_KEEP
				 POWER2:CJNE R3 ,#'*' ,ERRORRR
	           MOV A,R2
				MOV B,R2
				 MUL AB
				 MOV R0,A
				MOV A,B
				JNZ JUMP
				MOV A,R0
				LCALL LCD_NUM
				LCALL KEEP_KEEP
				
				ERRORRR:
	            ACALL ERROR_MSG
	            LJMP START
				JUMP:
	            LJMP  GREATER_THAN_255
	;=============================================== PROG FUNCTIONS =======================================================;
CHICK_INPUT:
	        MOV R1,A
			XRL A,#'/'
			JZ INPUT_DONE
			MOV A ,R1
			XRL A,#'*'
			JZ INPUT_DONE
			MOV A ,R1
			XRL A,#'-'
			JZ INPUT_DONE
			MOV A ,R1
			XRL A,#'+'
			JZ INPUT_DONE
			MOV A ,R1
			XRL A,#'='
			JZ INPUT_DONE
			MOV A ,R1
			XRL A,#'C'
			JNZ INPUT_DONE
			ACALL LCD_CLEAR
			LJMP START			
INPUT_DONE:
	        RET 
	
	
	
CHICK_OPR:
          MOV R1,A
		  XRL A,#'='
		  JNZ OPERATOR
		  MOV A,#'='
		  LCALL LCD_DATA
		  LCALL DELAY
		  MOV A,R2
		  LCALL LCD_NUM
		  LJMP KEEP
OPERATOR:		  
	      MOV A ,R1
		  XRL A,#'/'
		  JZ OPERATOR_DONE
		  MOV A ,R1 
		  XRL A,#'*'
		  JZ OPERATOR_DONE
		  MOV A ,R1
		  XRL A,#'-'
		  JZ OPERATOR_DONE
		  MOV A ,R1
		  XRL A,#'+'
		  JZ OPERATOR_DONE
		  
OPERATOR_DONE:
	      RET 
;======================================================ERROR _OVERFLOW==============================================================;	
ERROR_MSG:
	      ACALL LCD_CLEAR	
		  MOV DPTR ,#MSG
AGAIN_ERROR:
	      CLR A
		  MOVC A,@ A+DPTR
		  JZ	END_ERROR
		  ACALL LCD_DATA
		  ACALL DELAY
		  INC DPTR
		  SJMP AGAIN_ERROR
END_ERROR:
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	lCALL LCD_CLEAR
	RET
	
OVERFLOW_MSG:
	      ACALL LCD_CLEAR	
	      MOV DPTR ,#OVERFLOW
AGAIN_OVERFLOW:
         CLR A
	     MOVC A,@ A+DPTR
	     JZ	END_OVERFLOW
	     ACALL LCD_DATA
	     ACALL DELAY
	     INC DPTR
	     SJMP AGAIN_OVERFLOW
END_OVERFLOW:
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	ACALL DELAY
	lCALL LCD_CLEAR
	RET
;=========================================================================================================================================;	

;***************************************************** KEYPAD FUNCTIONS *****************************************************************;
	
	ORG 500H
		ROW1     : DB '7' , '8' , '9' , '/'
		ROW2     : DB '4' , '5' , '6' , '*'
		ROW3     : DB '1' , '2' , '3' , '-'
		ROW4     : DB 'C' , '0' , '=' , '+'
		MSG      : DB "ERROR",0	
	    OVERFLOW : DB"OVERFLOW",0
			
	READ_KEYPAD:
	K1: CLR RO1
	CLR RO2
	CLR RO3
	CLR RO4
	MOV A , KEYPAD
	ANL A , #0FH
	CJNE A , #00001111B , K2
	SJMP K1
	
	K2: ACALL DELAY_DEBOUNCE
	MOV A , KEYPAD
	ANL A , #0FH
	CJNE A , #00001111B , CHECK_ROW
	SJMP K1
	
	CHECK_ROW:
	CLR RO1
	SETB RO2
	SETB RO3
	SETB RO4
	MOV A , KEYPAD
	CJNE A , #01111111B , ROW_1
	
	CLR RO2
	SETB RO1
	SETB RO3
	SETB RO4
	MOV A , KEYPAD
	CJNE A , #10111111B , ROW_2
	
	CLR RO3
	SETB RO1
	SETB RO2
	SETB RO4
	MOV A , KEYPAD
	CJNE A , #11011111B , ROW_3
	
	CLR RO4
	SETB RO2
	SETB RO3
	SETB RO1
	MOV A , KEYPAD
	CJNE A , #11101111B , ROW_4
	
	LJMP K1
	
	ROW_1: MOV DPTR , #ROW1
	SJMP FIND
	ROW_2: MOV DPTR , #ROW2
	SJMP FIND
	ROW_3: MOV DPTR , #ROW3
	SJMP FIND
	ROW_4: MOV DPTR , #ROW4
	SJMP FIND
	
	FIND: MOV R7 , #4
	AGAIN1: RRC A
	JNC MATCH
	INC DPTR
	SJMP AGAIN1
	
	MATCH: CLR A
	MOVC A , @ A+DPTR
	
	RET
	
	
	DELAY_DEBOUNCE: MOV R5 , #255
	L1: MOV R6 , #255
	L2: DJNZ R6 , L2
	DJNZ R5 , L1
	RET
	
	
	
	
	;***** PROGRAMMING LCD *****

		
	LCD_INIT:
	MOV A , #01H
	ACALL LCD_COMM
	ACALL DELAY
	MOV A , #38H
	ACALL LCD_COMM
	ACALL DELAY
	MOV A , #0EH
	ACALL LCD_COMM
	ACALL DELAY
	RET
;=============================================================LCD_DISPLAY_RESULT=======================================================================;	
LCD_NUM: 
       
	    CJNE A ,#10, NOT_EQ1
		SJMP RESULT_2_DIGITS
NOT_EQ1:
        JNC NEXT1
		
RESULT_1_DIGITS:	
        ORL A ,#30H
		ACALL LCD_DATA
		ACALL DELAY
		SJMP EXT
NEXT1:  
        CJNE A ,#100, NOT_EQ2
	    SJMP RESULT_3_DIGITS
NOT_EQ2:
	    JNC NEXT2
RESULT_2_DIGITS:		
	    MOV B ,#10
	    DIV AB
		ORL A ,#30H
		ACALL LCD_DATA
		ACALL DELAY
		MOV A ,B
		ORL A ,#30H
		ACALL LCD_DATA
		ACALL DELAY
	    SJMP EXT
		
	
NEXT2:
       CJNE A ,#255, NOT_EQ3
        SJMP RESULT_3_DIGITS
NOT_EQ3:
	   JNC EXT
RESULT_3_DIGITS:
       MOV B ,#100
	   DIV AB
	   ORL A ,#30H
	   ACALL LCD_DATA
	   ACALL DELAY
	   MOV A ,B
	   MOV B ,#10
	   DIV AB
	   ORL A ,#30H
	   ACALL LCD_DATA
	   ACALL DELAY
	   MOV A ,B
	   ORL A ,#30H
	   ACALL LCD_DATA
	   ACALL DELAY
	   
EXT:   RET
	
LCD_COMM:
	   MOV LCD_PORT , A
	   CLR RS
	   SETB E
	   NOP
	   NOP
	   NOP
	   CLR E
	   RET
	
LCD_DATA:
	MOV LCD_PORT , A
	SETB RS
	SETB E
	NOP
	NOP
	NOP
	CLR E
	RET
	
LCD_CLEAR:
	MOV A , #01H
	ACALL LCD_COMM
	ACALL DELAY
	RET
	
	DELAY:
	MOV R5 , #50H
	LL1: MOV R6 , #0FFH
	LL2: DJNZ R6 , LL2
	DJNZ R5 , LL1
	RET
	
		
	END