FC := gfortran
FFLAGS := -Wall -O3 -cpp -g
PROGS := example_custom_hash_function example_multiple_tables	\
example_custom_types example_benchmark

.phony: all clean test

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

test: all
	@for prog in $(PROGS); do \
		echo "Running $$prog"; \
		./$$prog; \
	done

%: %.f90
	$(FC) -o $@ $< $(FFLAGS)

$(PROGS): ffhash_inc.f90
