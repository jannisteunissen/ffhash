FC := gfortran
FFLAGS := -Wall -O2 -cpp -g
PROGS := example_custom_hash_function example_multiple_tables example_custom_types example_benchmark

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $< $(FFLAGS)

$(PROGS): ffhash_inc.f90
