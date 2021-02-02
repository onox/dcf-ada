--  SPDX-License-Identifier: MIT
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

package DCF is
   pragma Pure;

   type Unsigned_8 is mod 2 ** 8
     with Size => 8;

   type Unsigned_16 is mod 2 ** 16
     with Size => 16;

   type Unsigned_32 is mod 2 ** 32
     with Size => 32;

   type Integer_64 is range -(2 ** 63) .. +(2 ** 63 - 1);
   --  Based on C99 long long int

   function Shift_Left (Value : Unsigned_32; Positions : Natural) return Unsigned_32 is
     (Value * 2 ** Positions);

   function Shift_Right (Value : Unsigned_32; Positions : Natural) return Unsigned_32 is
     (Value / 2 ** Positions);

end DCF;
