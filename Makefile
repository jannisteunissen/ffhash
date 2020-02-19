FC := gfortran
FFLAGS := -Wall -O2 -cpp -g
PROGS := example test benchmark_integer

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $< $(FFLAGS)

$(PROGS): ffhash_inc.f90
