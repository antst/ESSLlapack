ESSLlapack
==========

Complemental LAPACK for ESSL library.
For the routines defined in ESSL with standard LAPACK names, but with non-standard
calling sequences, it defines set of interfaces with standard LAPACK names.
For the routines missing in ESSL, it provides standard reference LAPACK 3.1.1 versions.

At allows to compile codes which require LAPACK routines without any modifications to the code,
and make use of optimized ESSL routines at the same time.

It works ONLY with shared ESSL and must be also compiled as shared library.
When code is linked with this library, "-lESSLlapack" must ALWAYS be in front of "-lESSL"
in set of linker arguments.

Brief idea behind this schema:
On load of shared library "libESSLlapack.so" it resolves entrance points for shared objects 
in libessl.so and store them in memory. When application calls routine with overlaping name, 
function if this library modify calling sequince and then pass arguments to ESSL version of it.

Installing
----------
1) Edit make.inc if nessesary

2) make

by default, serial and multithreaded version of ESSLlapack will be build and linked to corresnpondent ESSL libraries).

Donate
------
<a href='http://www.pledgie.com/campaigns/19516'><img alt='Click here to lend your support to: ESSLlapack donation and make a donation at www.pledgie.com !' src='http://www.pledgie.com/campaigns/19516.png?skin_name=chrome' border='0' /></a>

License terms
-------------
ESSLlapack is dual-licensed, as explained in [LICENSE](LICENSE).

