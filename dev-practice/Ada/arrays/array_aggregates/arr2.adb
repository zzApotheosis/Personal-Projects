--
-- Array aggregates.  Array aggregates are essentially constants of
-- array type.  They can be used either for initialization or in
-- assignment statements.
--
with Gnat.Io; use Gnat.Io;
procedure Arr2 is
   -- Just some values to play with, along with a conversion array.
   type Paint is (Red, Green, Yellow, Blue, Pink, Orange, Mauve,
                  Cherry, Indigo, Brown);
   PNames: array(Paint) of String(1..6) :=
     ("Red   ", "Green ", "Yellow", "Blue  ", "Pink  ", "Orange", "Mauve ",
      "Cherry", "Indigo", "Brown ");

   -- The type of an array of paints, along with its size.
   N: constant := 8;
   type AType is array(Integer range 1..N) of Paint;

   -- Some Paint arrays.  The first one is initialized with a list
   -- of colors.
   A: AType := (Red, Red, Pink, Blue, Orange, Cherry, Indigo, Indigo);
   B, C: AType;

   I: Integer;          -- Loop index.
begin
   -- Use positions to set varioius values in various places.
   B := (5 => Green, 2 => Orange, 6..8 => Indigo, 1|3|4 => Brown);

   -- Set the entire array to Blue.
   C := (AType'First .. AType'Last => Blue);

   -- Print the position numbers, spaced out to align with the
   -- the printouts of each of the arrays below.
   for I in 1..N loop
      Put("  ");
      Put(I);
      Put("    ");
   end loop;
   New_Line;

   -- Print out the contents of each of A, B, and C.
   for I in 1..N loop
      Put(PNames(A(I)) & " ");
   end loop;
   New_Line;

   for I in 1..N loop
      Put(PNames(B(I)) & " ");
   end loop;
   New_Line;

   for I in 1..N loop
      Put(PNames(C(I)) & " ");
   end loop;
   New_Line;
end Arr2;
