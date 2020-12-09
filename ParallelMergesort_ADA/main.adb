with text_io;
with Msort;

procedure main is
	use text_io;
	package Int_Io is new Integer_Io(Integer);
        use Int_Io;
	use Msort;

	--gloabl variables--
	A:Arr;
	total:Integer := 0;
	--------------------	
	task Reader is
		entry read_done;
	end;
	task Sum is
		entry start_sum;
	end;
	task Printer is
		entry start_print;
		entry print_sum;
	end;
	
	task body Reader is
		Input : Integer;
                idx:Integer := 1;
	begin
		while(not End_Of_File)
        	loop
                	Get(Input);
                	A(idx) := Input;
                	idx := idx + 1;
        	end loop;
		accept read_done;
	end Reader;

	task body Sum is
	begin
		accept start_sum;
		for idx in 1 .. LENGTH
		loop
			total := total + A(idx);
		end loop;
		Printer.print_sum;
	end Sum;

	task body Printer is
	begin
		accept start_print;
		for idx in 1 .. LENGTH
        	loop
                	Put(A(idx));
        	end loop;

		accept print_sum do
			Put(total);
		end print_sum;
	end Printer;

begin
	Reader.read_done;
	Sort(A);
	Sum.start_sum;
	Printer.start_print;
end main;
