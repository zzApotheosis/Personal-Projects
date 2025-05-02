--
-- Simple array example.
--
with Gnat.Io; use Gnat.Io;
procedure Arr1 is
   A: array(1..5) of Integer;   -- Array subscripts 1 to 5.
   I: Integer;
begin
   -- Read 'em in.
   for I in 1..5 loop
      Put("> ");
      Get(A(I));
   end loop;

   -- Put 'em out in reverse order.
   Put("[");
   for I in reverse A'Range loop
      Put(A(I));
      if I > A'First then
         Put(' ');
      end if;
   end loop;
   Put_Line("]");
end Arr1;
