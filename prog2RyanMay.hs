-------------------------------------------------------------
--------------------Programming Task 2-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------

--DEFINITIONS--
type Bit = Bool
type TwoBitSeq = (Bit, Bit)
type FourBitSeq = (Bit,Bit,Bit,Bit)
type FiveBitSeq = (Bit,Bit,Bit,Bit,Bit)
type EightBitFloat = (Bit,FiveBitSeq, TwoBitSeq)
---------------------------------------------------------------------------------------------------------

-- PROBLEM #1
-- a)
seq1011 :: FourBitSeq
seq1011 = (True,False,True,True)
-- b)
seq10110 :: FiveBitSeq
seq10110 = (True,False,True,True,False)
-- c)
seq10001 :: FiveBitSeq
seq10001 = (True,False,False,False,True)
-- d)
num1 :: EightBitFloat
num1 = (True, (True,False,True,True,False), (False,True))
-- e)
num2 :: EightBitFloat
num2 = (False, (False,False,False,True,False), (True,True))
-- f)
num3 :: EightBitFloat
num3 = (True, (False,False,False,False,False), (False,True))

---------------------------------------------------------------------------------------------------------
-- PROBLEM #2
-- Purpose:
--   bitVal b exp
--      Takes a bit value(ie, True or False which represents 1 or 0 respectively) and an integer that represents
--      a specific bit number, and returns the decimal value represented by that individual bit. Any bit that is false(i.e 0)
--      should return a zero value. This function also checks for negative exponents and returns 0 in that case.

-- Definition:
bitVal :: Bit -> Integer -> Integer
bitVal b exp
    | b == False || exp < 0 = 0
    | otherwise  = 2^exp

-- Tests:
--   Tests include testing with a false bit parameter, with 2 different exponents, to ensure they both return 0. Tests
--      also include testing with a true bit parameter, with 2 different exponents to ensure their proper corresponding
--      value is returned. Also tests for negative input, which is handled with returning a 0

t2a = bitVal False 1          -- Should return 0 (False bit passed in)
t2b = bitVal False 7          -- Should return 0 (False bit again, with different exponent passed in)
t2c = bitVal True 1           -- Should return 2 (True bit, with exponent of 1 i.e 2^1)
t2d = bitVal True 5           -- Should return 32 (True bit again, with different exponent passed in i.e. 2^5)
t2e = bitVal True 0           -- Should return 1 (True bit again, with zero as exponent i.e. 2^0)
t2f = bitVal True (-3)        -- Should return 0 (True bit with a negative exponent passed in, handled w return of 0)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #3
-- Purpose:
--   unsignedVal fourbit
--      Takes a FourBitSeq(4 bit binary num) and interprets the sequence as an unsigned bit sequence,
--      and returns the unsigned decimal value of the four bit sequence.

-- Definition:
unsignedVal :: FourBitSeq -> Integer
unsignedVal (a,b,c,d) = bitVal a 3 + bitVal b 2 + bitVal c 1 + bitVal d 0

-- Tests:
--   Tests include min and max of a 4 bit sequence (i.e. 0000 and 1111), along with other various 4 bit sequences.

t3a = unsignedVal (True,True,True,True)          -- Should return 15 (All 4 bits true, max unsigned val of 4 bits is 15)
t3b = unsignedVal (False,False,False,False)      -- Should return 0 (All four bits false, min unsigned val of 4 bits is 0)
t3c = unsignedVal (True,False,False,True)        -- Should return 9 (Only first and last bits True, i.e. 2^3 + 2^0)
t3d = unsignedVal (False,True,True,True)         -- Should return 7 (Last 3 bits True, i.e. 2^2 + 2^1 + 2^0)
t3e = unsignedVal (True, True, False, False)     -- Should return 12 (First 2 bits True, i.e 2^3 + 2^2)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #4
-- Purpose:
--   increment fourbit
--      Takes a FourBitSeq(4 bit binary num) and increments the sequence by 1.  This is accomplished by
--      pattern matching the fourbit sequences that come in.

-- Definition:
increment :: FourBitSeq -> FourBitSeq
increment (True,True,True,True) = (False,False,False,False)
increment (a,b,c,False)         = (a,b,c,True)
increment (a,b,False,True)      = (a,b,True,False)
increment (a,False,True,True)   = (a,True,False,False)
increment otherwise             = (True,False,False,False)

-- Tests:
--   Tests include min and max of a 4 bit sequence (i.e. 0000 and 1111), along with other various 4 bit sequences.

t4a = increment (True,True,True,True)          -- Should return (False,False,False,False) (All 4 bits true, when max val is incremented it returns 0)
t4b = increment (False,False,False,False)      -- Should return (False,False,False,True) (All four bits false, so zero incremented by 1)
t4c = increment (True,False,False,True)        -- Should return (True,False,True,False) (Only first and last bits True)
t4d = increment (False,True,True,True)         -- Should return (True,False,False,False) (Last 3 bits True)
t4e = increment (True, True, False, True)      -- Should return (True,True,True,False) (First 2 bits and last bit True)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #5
-- Purpose:
--   invertBits fourbit
--      Takes a FourBitSeq(4 bit binary num) and inverts each of the four bits in the sequence. That means any bit that
--         is True is inverted to False, and any bit that is False is inverted to True.

-- Definition: 
invertBits :: FourBitSeq -> FourBitSeq
invertBits (a,b,c,d) = (not a, not b, not c, not d)

-- Tests:
--   Tests include min and max of a 4 bit sequence (i.e. 0000 and 1111), along with other various 4 bit sequences.

t5a = invertBits (True,True,True,True)          -- Should return (False,False,False,False) (All 4 bits true, which should mean all false after inverted)
t5b = invertBits (False,False,False,False)      -- Should return (True,True,True,True) (All 4 bits false, which should mean all true after inverted)
t5c = invertBits (True,False,False,True)        -- Should return (False,True,True,False) (Only first and last bits True)
t5d = invertBits (False,True,True,True)         -- Should return (True,False,False,False) (Last 3 bits True)
t5e = invertBits (False, True, False, True)     -- Should return (True,False,True,False) (2nd and last bit true)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #6
-- Purpose:
--   signedVal fourbit
--      Takes a FourBitSeq(4 bit binary num) and interprets the sequence as an unsigned integer. Therefore if the
--         sequence has a leading 1(indicating a negative number), this function will calculate the two's complement to
--         return the negative number the sequence represents(done by inverting 4 bit sequence, incrementing by one, finding
--         the unsigned value, then multiplying by (-1)). If the sequence has a leading 0(indicating a positive number)
--         then the unsigned value of the sequence is calculated and returned.

-- Definition:
signedVal :: FourBitSeq -> Integer
signedVal bits@(False,_,_,_) = unsignedVal bits
signedVal bits@(True,_,_,_) = (unsignedVal(increment (invertBits bits))) * (-1)

-- Tests:
--   Tests include a collection of sequences with a leading True(or 1) indicating neg numbers, and a collection of sequences
--      with a leading False(or 0) indicating non negative numbers. Includes min and max of an unsigned 4 bit sequence

t6a = signedVal (True,True,True,True)          -- Should return -1 (Leading True bit, neg number)
t6b = signedVal (False,False,False,False)      -- Should return 0  (Leading False bit, non negative number)
t6c = signedVal (True,False,False,False)       -- Should return -8 (A different sequence with leading True bit, Min of 4 bit unsigned)
t6d = signedVal (False,True,True,True)         -- Should return 7 (A different sequence with leading False bit, Max of 4 bit unsigned)
t6e = signedVal (False,True,False,True)        -- Should return 5 (A different sequence with leading False bit)
t6f = signedVal (True,False,False,True)        -- Should return -7 (A different sequence with leading True bit)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #7
-- Purpose:
--   significandVal twoBit norm
--      Takes a TwoBitSeq(2 bit binary num) and determines the significand value of the TwoBitSeq.  The function also takes
--         a boolean that indicates whether the returned value should be normalized or not (where normalized means the value
--         returned will have a leading 1 ahead of the fractional)

-- Definition:
significandVal :: TwoBitSeq -> Bool -> Float
significandVal (True,True) False = 3/4
significandVal (False,True) False = 1/4
significandVal (True, False) False = 1/2
significandVal (False, False) False = 0
significandVal bits True = 1 + significandVal bits False

-- Tests:
--   Because we are dealing with 3 different booleans, there are 8 possible inputs total.  Given a small amount of total possibilities
--      These tests will include all 8 possible inputs

t7a = significandVal (True,True) False       -- Should return 0.75 (Both sig bits true, NOT normalized)
t7b = significandVal (False,True) False      -- Should return 0.25 (Second sig bit true, NOT normalized)
t7c = significandVal (True,False) False      -- Should return 0.50 (First sig bit true, NOT normalized)
t7d = significandVal (False,False) False     -- Should return 0.00 (Both sig bits false, NOT normalized)
t7e = significandVal (True,True) True        -- Should return 1.75 (Both sig bits true, normalized)
t7f = significandVal (False,True) True       -- Should return 1.25 (Second sig bit true, normalized)
t7g = significandVal (True,False) True       -- Should return 1.50 (First sig bit true, normalized)
t7h = significandVal (False,False) True      -- Should return 1.00 (Both sig bits false, normalized)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #8
-- Purpose:
--   exponentVal fiveBit
--      Takes a FiveBitSeq(5 bit binary num) uses the sequence to calculate the exponent field. This is done by
--         evaluating the 5 bit sequence as an unsigned integer and subtracting 15(exponent bias) from the result.
--         Also accounts for the possibility of representing a denormalized number, which happens when all the bits
--         of the FiveBitSeq parameter are false.  The function returns a pair that includes the exponent value, and
--         a boolean indicating true for normalized numbers and false for denormalized numbers.

-- Definition:
exponentVal :: FiveBitSeq -> (Float,Bool)
exponentVal (False,False,False,False,False) = (-14,False)
exponentVal (a,b,c,d,e) = (fromIntegral(bitVal a 4 + bitVal b 3 + bitVal c 2 + bitVal d 1 + bitVal e 0 - bias),True)
    where bias = 15

-- Tests:
--   Tests will include passing in  a FiveBitSeq of all False bits, which indicates denormalized number. Tests also
--       include other various FiveBitSeq that indicate normalized numbers. All cases excluding all 5 false bits, should
--       return a pair including the unsigned val of the FiveBitSeq and a boolean of True.

t8a = exponentVal (False,False,False,False,False)       -- Should return (-14.0,False) (All False bits, indicates NOT normalized, return w -14)
t8b = exponentVal (True,True,True,True,True)            -- Should return (16.0,True) (All True bits, indicates normalized, 31-15 = 16)
t8c = exponentVal (False,True,False,False,False)        -- Should return (-7.0,True) (different 5bitSeq, indicates normalized, 8-15 = -7)
t8d = exponentVal (True,True,False,False,True)          -- Should return (10.0,True) (different 5bitSeq, indicates normalized, 25-15 = 10)
t8e = exponentVal (False,False,False,False,True)        -- Should return (-14.0,True) (different 5bitSeq, indicates normalized, 1-15 = -14)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #9
-- Purpose:
--   exponentVal fiveBit
--      Takes an EightBitSeq as a parameter and calculates and returns the floating point number that the EightBitSeq represents.
--      The first bit indicates if the number is pos or neg, the next five bits are used to calculate the exponent field, while the
--      final two bits are used to calculate the significand value of the floating point representation.

-- Definition:
floatVal :: EightBitFloat -> Float
floatVal (False,(exp),(sig)) = coefficient * (2 ** ex)
    where 
      coefficient = significandVal sig norm
      (ex,norm) = (exponentVal exp)
floatVal (True,(exp),(sig)) = -1 * floatVal (False,exp,sig)

-- Tests:
--   Tests will include negative and positive floating point numbers.  The tests will also include both normalized and denormalized
--      representations of floating point numbers, including negative and positive tests for both norm and denorm.

t9a = floatVal (True,(False,False,False,False,False),(True,False))       -- Should return -3.0517578e-5 (Denormalized negative number)
t9b = floatVal (False,(False,False,False,False,False),(True,False))      -- Should return 3.0517578e-5 (Same as t9a, but positive)
t9c = floatVal (True,(False,False,True,False,True),(False,False))        -- Should return -9.765625e-4 (Normalized negative number, both sig bits false)
t9d = floatVal (False,(False,False,True,False,True),(False,False))       -- Should return 9.765625e-4 (Same as t9c, but positive)
t9e = floatVal (False,(False,False,True,False,True),(True,True))         -- Should return 1.7089844e-3 (Same as t9d, but both sig bits true)
t9f = floatVal (False,(True,True,True,False,True),(True,True))           -- Should return 28672.0  (Normalized number with positive exponent)