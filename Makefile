FC := gfortran
FFLAGS := -Wall -O2 -fcheck=all -cpp -g
PROGS := test

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $< $(FFLAGS)

$(PROGS): m_khash.f90
