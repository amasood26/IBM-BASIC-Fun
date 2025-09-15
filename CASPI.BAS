10 REM --- PI Calculation for IBM Cassette BASIC ---
20 REM --- Target: IBM 5150 ROM BASIC ---
30 REM --- Uses Machin: PI = 16*arctan(1/5) - 4*arctan(1/239)
40 REM --- *** WARNING: EXTREMELY SLOW on original hardware! ***
50 CLS
60 PRINT "Calculating Pi to 100 digits..."
70 PRINT "(This will take MANY HOURS or even DAYS on a real 5150!)"

80 REM --- Setup ---
90 NDIGITS% = 100 ' Desired number of decimal digits
100 NCALC% = NDIGITS% + 10 ' Number of digits to calculate (guard digits)
110 BASE% = 10000 ' Work in base 10000 (4 decimal digits per element)
120 LEN% = INT(NCALC% / 4) + 2 ' Number of array elements needed
130 DIM P(LEN%), A(LEN%), B(LEN%), T(LEN%), Z(LEN%) ' P=Pi, A/B=Arctans, T=Term, Z=Zero/Temp

140 REM --- Calculate 16 * Arctan(1/5) ---
150 X% = 5 : GOSUB 2000 ' Arctan(1/5) -> store in A()
160 REM A() now holds arctan(1/5), T() is a copy
170 F% = 16 : GOSUB 3000 ' Multiply A() by 16 -> result in T()
180 FOR I% = 0 TO LEN%: P(I%) = T(I%): NEXT I% ' Copy 16*arctan(1/5) to P()

190 REM --- Calculate 4 * Arctan(1/239) ---
200 X% = 239 : GOSUB 2000 ' Arctan(1/239) -> store in B() (T is modified inside)
210 REM B() now holds arctan(1/239), T() is a copy
220 F% = 4 : GOSUB 3000 ' Multiply B() by 4 -> result in T()
230 REM P() holds 16*arctan(1/5)
240 REM T() holds 4*arctan(1/239)

250 REM --- Subtract: P = P - T ---
260 GOSUB 4000 ' Subtract T() from P() -> result in P()

270 REM --- Print Result ---
280 PRINT
290 PRINT "Pi = ";
300 PRINT INT(P(LEN%)); "."; ' Print the integer part (should be 3)
310 FOR I% = LEN% - 1 TO 1 STEP -1
320   NUMSTR$ = STR$(P(I%)) ' Get number as string (e.g., " 1234", " 56")
330   IF LEFT$(NUMSTR$, 1) = " " THEN NUMSTR$ = MID$(NUMSTR$, 2) ' Remove leading space
340   PRINT RIGHT$("0000" + NUMSTR$, 4); ' Pad with zeros and print 4 digits
350 NEXT I%
360 PRINT
370 PRINT "Calculation complete."
380 END ' End of main program

1000 REM --- *** ARITHMETIC SUBROUTINES (Cassette BASIC compatible) ***

1010 REM --- Sub: Clear Array Z() to Zero ---
1020 FOR I% = 0 TO LEN%: Z(I%) = 0: NEXT I%
1030 RETURN

1040 REM --- Sub: Divide Array N() by integer D%, result in T() ---
1050 REM --- Input: N() is used implicitly by context (A, B, or T), D% = Divisor
1060 REM --- Output: T() holds the result. Uses Z() internally.
1070 GOSUB 1010 ' Clear Z() for result
1080 C = 0 ' Carry/Remainder (single precision needed)
1090 FOR I% = LEN% TO 0 STEP -1
1100   CR = C * BASE% + N(I%) ' Current value (can exceed 32767)
1110   Q = INT(CR / D%) ' Quotient (single precision intermediate ok)
1120   C = CR - D% * Q ' Remainder (C = CR MOD D%)
1130   Z(I%) = Q ' Store quotient in Z()
1140 NEXT I%
1150 FOR I% = 0 TO LEN%: T(I%) = Z(I%): NEXT I% ' Copy result Z() to T()
1160 RETURN

1170 REM --- Sub: Subtract B() from A(), result in A() ---
1180 BORROW = 0 ' Single precision okay
1190 FOR I% = 0 TO LEN%
1200   DIFF = A(I%) - B(I%) - BORROW
1210   IF DIFF >= 0 THEN GOTO 1250 ' Skip borrow logic if non-negative
1220   REM Handle borrow
1230   A(I%) = DIFF + BASE%
1240   BORROW = 1 : GOTO 1270 ' Go to next iteration
1250   REM No borrow needed
1260   A(I%) = DIFF
1270   BORROW = 0 ' Reset borrow for this case (or remains 0 if skipped)
1280 NEXT I%
1290 RETURN

1300 REM --- Sub: Add B() to A(), result in A() ---
1310 CARRY = 0 ' Single precision okay
1320 FOR I% = 0 TO LEN%
1330   SUM = A(I%) + B(I%) + CARRY
1340   REM A(I%) = SUM MOD BASE%
1350   A(I%) = SUM - BASE% * INT(SUM / BASE%)
1360   REM CARRY = SUM \ BASE%
1370   CARRY = INT(SUM / BASE%)
1380 NEXT I%
1390 RETURN

1400 REM --- Sub: Check if Array Z() is Zero ---
1410 REM --- Sets Z(0) = -1 if zero, otherwise Z(0) is unchanged (>=0)
1420 Z(0) = 0 ' Assume non-zero initially for flag check logic
1430 FOR I% = LEN% TO 0 STEP -1 ' Check from most significant down
1440   IF Z(I%) <> 0 THEN RETURN ' Non-zero found, exit early
1450 NEXT I%
1460 REM If loop finishes, array is zero
1470 Z(0) = -1 ' Set flag indicating zero
1480 RETURN

2000 REM --- *** Calculate Arctan(1/X%) into Array passed implicitly ***
2010 REM --- Result stored in T(), original X% determines which array (A or B) gets sum
2020 REM --- Series: arctan(1/x) = 1/x - 1/(3x^3) + 1/(5x^5) - ...
2030 REM --- X% should be set before calling (5 or 239)
2040 X2% = X% * X% ' Precalculate X squared

2050 REM --- Initialize Sum (in A() or B()) and First Term (in T()) ---
2060 GOSUB 1010 ' Clear Z() to use as term accumulator / temp
2070 Z(LEN%) = 1 ' Start with 1 for initial division
2080 REM Assign N() for division context - Use Z() as N()
2090 FOR I%=0 TO LEN%: N(I%)=Z(I%) : NEXT I%
2100 D% = X% : GOSUB 1040 ' Divide Z (1) by X% -> Result in T() (First term 1/X%)
2110 IF X% = 5 THEN FOR I%=0 TO LEN%: A(I%)=T(I%): NEXT I% ELSE FOR I%=0 TO LEN%: B(I%)=T(I%): NEXT I%
2120 REM A() or B() now holds the first term (the initial sum)
2130 REM T() also holds the current term value

2140 K% = 1 ' Term counter (denominator odd number)
2150 SIGN% = -1 ' Sign of next term

2160 REM --- Loop for Taylor series terms ---
2170   K% = K% + 2 ' Next odd number (3, 5, 7...)
2180   REM Divide current term T() by X2% -> store result back in T()
2190   FOR I%=0 TO LEN%: N(I%)=T(I%) : NEXT I% ' Set N() context for division
2200   D% = X2% : GOSUB 1040 ' Divide T() by X2% -> Result T()
2210   REM Copy T() to B() temporarily FOR division by K% (using B as temp N())
2220   FOR I% = 0 TO LEN%: N(I%) = T(I%): NEXT I% ' N() is now T()
2230   D% = K% : GOSUB 1040 ' Divide N() (which was T()) by K% -> Result T()
2240   REM T() now holds the value of the next term in the series

2250   REM --- Check if term T() is effectively zero ---
2260   FOR I% = 0 TO LEN%: Z(I%) = T(I%): NEXT I% ' Copy term to Z for check
2270   GOSUB 1400 ' Check if Z is zero (sets Z(0)=-1 if true)
2280   IF Z(0) = -1 THEN GOTO 2350 ' Term is zero, exit loop

2290   REM --- Add or subtract term T() from sum (A() or B()) ---
2300   REM We need B() to hold the term T() for add/subtract routines
2310   FOR I% = 0 TO LEN%: B(I%) = T(I%): NEXT I%
2320   IF X% = 5 THEN GOSUB 2400 ELSE GOSUB 2500 ' Branch to add/sub for A or B

2330   SIGN% = -SIGN% ' Flip sign for next term
2340 GOTO 2170 ' Loop back for next term

2350 REM --- Loop finished ---
2360 REM Result sum is in A() or B() depending on X%
2370 REM T() needs to hold a copy of the result for the main part of the code
2380 IF X% = 5 THEN FOR I%=0 TO LEN%: T(I%)=A(I%): NEXT I% ELSE FOR I%=0 TO LEN%: T(I%)=B(I%): NEXT I%
2390 RETURN

2400 REM --- Sub-part: Add/Subtract Term B() from Sum A() ---
2410 IF SIGN% = 1 THEN GOSUB 1300 : RETURN ' Add B() to A()
2420 REM Else subtract
2430 GOSUB 1170 ' Subtract B() from A()
2440 RETURN

2500 REM --- Sub-part: Add/Subtract Term B() from Sum B() ---
2510 REM This path is for Arctan(1/239), sum is stored in B()
2520 REM Need to copy B() to A() temporarily for the SUB/ADD routines
2530 FOR I%=0 TO LEN%: A(I%)=B(I%): NEXT I% ' Temp copy sum B -> A
2540 REM Now B() still holds the term to add/subtract
2550 IF SIGN% = 1 THEN GOSUB 1300 ' Add B() to A() (which holds sum)
2560 IF SIGN% <> 1 THEN GOSUB 1170 ' Subtract B() from A() (which holds sum)
2570 FOR I%=0 TO LEN%: B(I%)=A(I%): NEXT I% ' Copy result back A -> B
2580 RETURN


3000 REM --- *** Multiply Array A() (or B()) by integer F%, result in T() ***
3010 REM --- F% should be set before calling (16 or 4)
3020 REM --- Source array A() or B() determined by F% for this specific script
3030 GOSUB 1010 ' Clear Z() for result
3040 C = 0 ' Carry (single precision)
3050 FOR I% = 0 TO LEN%
3060   IF F% = 16 THEN PROD = A(I%) * F% + C ELSE PROD = B(I%) * F% + C
3070   REM Z(I%) = PROD MOD BASE%
3080   Z(I%) = PROD - BASE% * INT(PROD / BASE%)
3090   REM C = PROD \ BASE%
3100   C = INT(PROD / BASE%)
3110 NEXT I%
3120 FOR I% = 0 TO LEN%: T(I%) = Z(I%): NEXT I% ' Copy result Z() to T()
3130 RETURN

4000 REM --- *** Subtract T() from P(), result in P() ***
4010 BORROW = 0 ' Single precision okay
4020 FOR I% = 0 TO LEN%
4030   DIFF = P(I%) - T(I%) - BORROW
4040   IF DIFF >= 0 THEN GOTO 4080 ' Skip borrow logic if non-negative
4050   REM Handle borrow
4060   P(I%) = DIFF + BASE%
4070   BORROW = 1 : GOTO 4110 ' Go to next iteration
4080   REM No borrow needed
4090   P(I%) = DIFF
4100   BORROW = 0 ' Reset borrow for this case (or remains 0 if skipped)
4110 NEXT I%
4120 RETURN
