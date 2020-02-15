FC := gfortran
FFLAGS := -Wall -O2 -cpp -g
PROGS := test benchmark_integer

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $< $(FFLAGS)

$(PROGS): m_khash.f90
