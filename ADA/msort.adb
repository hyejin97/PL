with Text_Io;
use Text_Io;

package body Msort is
	package Int_Io is new Integer_Io(Integer);
   	use Int_Io;

	procedure ParallelMSort(A:in out Arr; l:Integer; u:Integer) is

		mid : Integer := (l + u) / 2;

		procedure Partition is
			task left;
			task right;

			task body left is
			begin
				ParallelMSort(A, l, mid);
			end left;

			task body right is
			begin
				ParallelMSort(A, mid+1, u);
			end right;
		begin
			null;
		end Partition;

		procedure Merge is
			AL : array(1 .. mid - l + 1) of Integer;
			AR : array(1.. u - mid) of Integer;
			i,j : Integer := 1;
			k : Integer := l;
		begin
			for idx in 1 .. mid - l + 1
			loop
				AL(idx) := A(l + idx - 1);
			end loop;
			for idx in 1 .. u - mid
			loop
				AR(idx) := A(mid + idx);
			end loop;
			while i <= mid - l + 1 and j <= u - mid
			loop
				if AL(i) <= AR(j) then
					A(k) := AL(i);
					i := i + 1;
				else
					A(k) := AR(j);
					j := j + 1;
				end if;
				k := k + 1;
			end loop;

			while i <= mid - l + 1
			loop
				A(k) := AL(i);
				i := i + 1;
				k := k + 1;
			end loop;

			while j <= u - mid
			loop
				A(k) := AR(j);
				j := j + 1;
				k := k + 1;
			end loop;
		end Merge;
	begin
		if l < u then
			Partition;
			Merge;
		end if;
	end ParallelMSort;

	procedure Sort(A:in out Arr) is
	begin
		ParallelMSort(A, 1, LENGTH);
	end Sort;


end Msort;
