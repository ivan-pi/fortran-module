FC = gfortran

all: precice precice_standard precice_nonstandard

precice: precice.f90
	$(FC) -c -Wall -fimplicit-none $^

precice_standard: precice_standard.f90
	$(FC) -c -Wall -fimplicit-none $^

precice_nonstandard: precice_nonstandard.f90
	$(FC) -c -Wall -fimplicit-none $^

clean:
	rm -f *.mod *.o

