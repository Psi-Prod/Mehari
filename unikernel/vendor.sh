#!/bin/bash

[ ! -d "vendors" ] && mkdir vendors
[ ! -d "vendors/mkernel" ] && opam source mkernel --dir vendors/mkernel
[ ! -d "vendors/mnet" ] && opam source mnet --dir vendors/mnet
[ ! -d "vendors/mirage-crypto-rng-mkernel" ] && opam source mirage-crypto-rng-mkernel --dir vendors/mirage-crypto-rng-mkernel
[ ! -d "vendors/mirage-ptime" ] && opam source mirage-ptime --dir vendors/mirage-ptime
[ ! -d "vendors/bstr" ] && opam source bstr --dir vendors/bstr
[ ! -d "vendors/gmp" ] && opam source gmp --dir vendors/gmp
[ ! -d "vendors/cstruct" ] && opam source cstruct --dir vendors/cstruct
[ ! -d "vendors/digestif" ] && opam source digestif --dir vendors/digestif
[ ! -d "vendors/hxd" ] && opam source hxd --dir vendors/hxd
[ ! -d "vendors/kdf" ] && opam source kdf --dir vendors/kdf
[ ! -d "vendors/ipaddr" ] && opam source ipaddr --dir vendors/ipaddr
[ ! -d "vendors/utcp" ] && opam source utcp --dir vendors/utcp
[ ! -d "vendors/tls" ] && opam source tls --dir vendors/tls
[ ! -d "vendors/zarith" ] && opam source zarith --dir vendors/zarith
[ ! -d "vendors/x509" ] && opam source x509 --dir vendors/x509
