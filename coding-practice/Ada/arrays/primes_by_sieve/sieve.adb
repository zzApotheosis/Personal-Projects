--
-- Print out prime numbers using the seive method.  The program requests
-- a maximum and prints out all the primes up to the maximum.
--
with Gnat.Io; use Gnat.Io;
procedure Sieve is
   -- Type of sieves.
   type Sieve_Arr_Type is array(Integer range <>) of Boolean;
   Max: Integer;                -- Max integer to search.
begin
   Put("Prime Search Maximum: ");
   Get(Max);

   -- This introduces new declarations.  Their scope is through the
   -- end of the block.
   declare
      -- Max number of lines.
      Line_Max: constant := 12;

      -- Sieve itself.  The contents are initialized to true.
      IsPrime: Sieve_Arr_Type(2..Max) := (2..Max => True);

      Maybe_Prime,              -- Scan through possible primes.
        Multiple: Integer;      -- Scan through multiples of primes.
      Line_Count: Integer;      -- Number of items output on curr line.
   begin
      -- Go through each integer and see if we know it to be prime.
      -- If it is, eliminate all its multiples.
      for Maybe_Prime in 2..Max loop

         -- Is Maybe_Prime really prime?
         if IsPrime(Maybe_Prime) then

            -- Eliminate the multiples or Maybe_Prime, which are not prime.
            Multiple := 2*Maybe_Prime;
            while Multiple <= Max loop
               IsPrime(Multiple) := False;
               Multiple := Multiple + Maybe_Prime;
            end loop;

         end if;
      end loop;

      -- Go through the numbers and print the prime ones.
      Line_Count := 0;
      for Maybe_Prime in 2..Max loop
         -- Is it _really_ prime?
         if IsPrime(Maybe_Prime) then
            -- If the line is full, start another.  Otherwise, just
            -- issue a space to separate.
            if Line_Count >= Line_Max then
               New_Line;
               Line_Count := 0;
            else
               Put(" ");
            end if;

            -- Print the prime and count the fact.
            Put(Maybe_Prime);
            Line_Count := Line_Count + 1;
         end if;
      end loop;

      -- If we didn't finish the last line, do so.
      if Line_Count > 0 then
         New_Line;
      end if;
   end;
end Sieve;
