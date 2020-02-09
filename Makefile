FC := gfortran
FFLAGS := -Wall -O2 -fcheck=all -cpp
PROGS := test

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $^ $(FFLAGS)
