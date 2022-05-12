# Kerfuffle and Quagmire?
These languages are a subset of the CMSC430 language Knock and Neerdowell that provide the basics of gradual typing. 

# How to use?
1. First remove CMSC430's lang package with ``raco pkg remove langs``
2. Next install our package with ``raco pkg install 'https://github.com/CMSC838E-Project/a86.git?path=langs#main'``
3. Now simply ``cd`` into one of the languages and you can:
* Run the tests using ``raco test test/compiler.rkt`` or ``raco test test/interp.rkt``
* Run your own program using ``make clean``, ``make [filename].run``, and ``./[filename].run``
