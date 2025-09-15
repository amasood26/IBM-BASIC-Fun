10 REM --- PI Calculation to 100+ Digits in IBM BASIC ---
20 REM --- Uses Machin's Formula: PI = 16*arctan(1/5) - 4*arctan(1/239)
30 REM --- Warning: This is VERY SLOW in interpreted BASIC! ---
40 CLS
50 PRINT "Calculating Pi to 100 digits..."
60 PRINT "This will take a long time..."

70 REM --- Setup ---
80 DEFINT A-Z ' Default variables to integer for potential speedup
90 NDIGITS = 100 ' Desired number of decimal digits
100 N = NDIGITS + 10 ' Number of digits to calculate (add guard digits)
110 BASE = 10000 ' Work in base 10000 (4 decimal digits per element)
120 LEN = N / 4 + 2 ' Number of array elements needed
130 DIM P(LEN), A(LEN), B(LEN), T(LEN), Z(LEN) ' P=Pi, A=arctan(1/5), B=arctan(1/239), T=Term, Z=Zero check

140 REM --- Subroutine: Calculate Arctan(1/X) ---
150 REM --- Result stored in array T()
160 GOSUB 2000 ' Arctan(1/5) -> store in A()
170 FOR I = 0 TO LEN: A(I) = T(I): NEXT I
180 GOSUB 3000 ' Multiply A() by 16 -> store in P()
190 FOR I = 0 TO LEN: P(I) = T(I): NEXT I

200 GOSUB 2000 ' Arctan(1/239) -> store in B() (using X=239 set inside)
210 FOR I = 0 TO LEN: B(I) = T(I): NEXT I
220 GOSUB 3000 ' Multiply B() by 4 -> store in T()
230 REM --- P() currently holds 16*arctan(1/5)
240 REM --- T() currently holds 4*arctan(1/239)
250 GOSUB 4000 ' Subtract T() from P() -> store result in P()

260 REM --- Print Result ---
270 PRINT
280 PRINT "Pi = ";
290 PRINT P(LEN); "."; ' Print the integer part (should be 3)
300 FOR I = LEN - 1 TO 1 STEP -1
310 PRINT RIGHT$("000" + LTRIM$(STR$(P(I))), 4); ' Print 4 digits, padding with zeros
320 NEXT I
330 PRINT
340 PRINT "Calculation complete."
350 END ' End of main program

1000 REM --- *** ARITHMETIC SUBROUTINES ***

1010 REM --- Sub: Clear Array Z() to Zero ---
1020 FOR I = 0 TO LEN: Z(I) = 0: NEXT I
1030 RETURN

1040 REM --- Sub: Divide Array N() by integer D, result in Q() ---
1050 REM --- Uses T() internally for temporary storage
1060 GOSUB 1010 ' Clear Z() which will be used for Q() initially
1070 C = 0 ' Carry (or remainder)
1080 FOR I = LEN TO 0 STEP -1
1090   CR = C * BASE + N(I) ' Current value to divide
1100   Q = CR \ D ' Integer quotient for this element
1110   C = CR MOD D ' Remainder becomes carry for next element
1120   Z(I) = Q ' Store quotient in Z()
1130 NEXT I
1140 FOR I = 0 TO LEN: T(I) = Z(I): NEXT I ' Copy result Z() to T()
1150 RETURN

1160 REM --- Sub: Subtract B() from A(), result in A() ---
1170 BORROW = 0
1180 FOR I = 0 TO LEN
1190   DIFF = A(I) - B(I) - BORROW
1200   IF DIFF < 0 THEN
1210     A(I) = DIFF + BASE
1220     BORROW = 1
1230   ELSE
1240     A(I) = DIFF
1250     BORROW = 0
1260   END IF
1270 NEXT I
1280 RETURN

1290 REM --- Sub: Add B() to A(), result in A() ---
1300 CARRY = 0
1310 FOR I = 0 TO LEN
1320   SUM = A(I) + B(I) + CARRY
1330   A(I) = SUM MOD BASE
1340   CARRY = SUM \ BASE
1350 NEXT I
1360 RETURN

1370 REM --- Sub: Check if Array Z() is Zero ---
1380 FOR I = 0 TO LEN
1390   IF Z(I) <> 0 THEN RETURN ' Non-zero found
1400 NEXT I
1410 REM If loop finishes, array is zero
1420 Z(0) = -1 ' Use Z(0) as a flag for zero (quick check)
1430 RETURN

2000 REM --- *** Calculate Arctan(1/X) into T() ***
2010 REM --- Series: arctan(1/x) = 1/x - 1/(3x^3) + 1/(5x^5) - ...
2020 IF A(LEN) = 0 THEN X = 5 ELSE X = 239 ' Choose X based on which call this is (crude check)
2030 X2 = X * X ' Precalculate X squared

2040 GOSUB 1010 ' Clear Z() to use as term accumulator
2050 Z(LEN) = 1 ' Start with 1 in the most significant place for initial division
2060 GOSUB 1040 ' Divide Z (which is 1) by X -> Result in T() (This is the first term 1/X)
2070 FOR I = 0 TO LEN: A(I) = T(I): NEXT I ' Copy first term T() to A() which holds the sum

2080 K = 1 ' Term counter (denominator odd number)
2090 SIGN = -1 ' Sign of next term

2100 REM --- Loop for Taylor series terms ---
2110   K = K + 2 ' Next odd number (3, 5, 7...)
2120   GOSUB 1040 ' Divide current term T() by X2 -> result in T()
2130   FOR I = 0 TO LEN: B(I) = T(I): NEXT I ' Copy T() to B() temporarily
2140   GOSUB 1040 ' Divide B() by K -> result in T() (This is the term value)

2150   REM --- Check if term T() is effectively zero ---
2160   FOR I = 0 TO LEN: Z(I) = T(I): NEXT I
2170   GOSUB 1370
2180   IF Z(0) = -1 THEN GOTO 2250 ' Term is zero, exit loop

2190   REM --- Add or subtract term T() from sum A() ---
2200   IF SIGN = 1 THEN
2210     GOSUB 1290 ' Add T() to A()
2220   ELSE
2230     GOSUB 1160 ' Subtract T() from A()
2240   END IF
2250   SIGN = -SIGN ' Flip sign for next term
2260 GOTO 2110 ' Loop back for next term
2270 REM --- Loop finished, result is in A() ---
2280 FOR I = 0 TO LEN: T(I) = A(I): NEXT I ' Copy final sum A() to T() for return
2290 RETURN

3000 REM --- *** Multiply Array A() by integer F, result in T() ***
3010 IF P(LEN) = 0 THEN F = 16 ELSE F = 4 ' Determine factor (16 or 4) - crude check
3020 GOSUB 1010 ' Clear Z() for result
3030 C = 0 ' Carry
3040 FOR I = 0 TO LEN
3050   PROD = A(I) * F + C
3060   Z(I) = PROD MOD BASE
3070   C = PROD \ BASE
3080 NEXT I
3090 FOR I = 0 TO LEN: T(I) = Z(I): NEXT I ' Copy result Z() to T()
3100 RETURN

4000 REM --- *** Subtract B() (which is T()) from A() (which is P()), result in P() ***
4010 BORROW = 0
4020 FOR I = 0 TO LEN
4030   DIFF = P(I) - T(I) - BORROW
4040   IF DIFF < 0 THEN
4050     P(I) = DIFF + BASE
4060     BORROW = 1
4070   ELSE
4080     P(I) = DIFF
4090     BORROW = 0
4100   END IF
4110 NEXT I
4120 RETURN
