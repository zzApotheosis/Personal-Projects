--
-- This reads in a string on a line, but skips leading blanks.
--
with Gnat.Io; use Gnat.Io;
procedure ReadRest is
   Max: constant := 20;         -- Max size of string.
   Fred: String(1..Max);        -- String.
   I: Integer;                  -- Input subscript.
   Ch: Character;               -- Search for first char.
begin
   -- Read to first non-blank character and put it into the first
   -- position of the array.
   loop
      Get(Ch);
      exit when Ch /= ' ';
   end loop;
   Fred(1) := Ch;

   -- Read the rest of the line into the part of the array after the
   -- first position.
   Get_Line(Fred(2..Fred'last), I);

   -- Echo the whole string, leading blanks trimmed.
   Put_Line(Fred(1..I));
end ReadRest;
