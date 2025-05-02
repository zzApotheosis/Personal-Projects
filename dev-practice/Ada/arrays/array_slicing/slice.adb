--
-- Array slices.
--
with Gnat.Io; use Gnat.Io;
procedure Slice is
   -- Saome random arrays.
   type A3 is array(3..5) of Integer;
   A: array(1..6) of Integer := (5, 11, 3, 52, 2, 99);
   B: A3;

   I: Integer;          -- Loop index

   -- Random string.
   S: String := "Hi there.  How are you?";

   -- Some random arrays of the same base type.
   type An is array(Integer range <>) of Integer;
   C: An(1..6) := (22, 9, 1, 45, 33, 11);
   D: An(3..5);

begin
   -- This copies a portion of A to B.  Notice that the conversion is
   -- required because A and B have different types.
   B := A3(A(2..4));
   for I in 3..5 loop
      Put(B(I));
      Put(" ");
   end loop;
   New_Line;

   -- This copies a different portion of A to B.
   B := A3(A(4..6));
   for I in 3..5 loop
      Put(B(I));
      Put(" ");
   end loop;
   New_Line;

   -- This copies a portion of A to another.  Notice that slices can
   -- appear on the left.
   A(1..3) := A(4..6);
   for I in 1..6 loop
      Put(A(I));
      Put(" ");
   end loop;
   New_Line;

   -- This prints some substrings of S.  Substrings are just s just
   -- slices of strings, which are just arrays.
   Put_Line(S);
   Put_Line(S(1..2));
   Put_Line(S(5..9));
   Put_Line(S(8..20));

   -- This copies a portion of C to D.  Notice that the conversion is not
   -- required, because C and D are of the same type (type An), even though
   -- they have different sizes.
   D := C(2..4);
   for I in 3..5 loop
      Put(D(I));
      Put(" ");
   end loop;
   New_Line;
end Slice;
