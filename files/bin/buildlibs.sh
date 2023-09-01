#!/bin/bash

ARCH=$(uname -m)
OS=$(uname -o)
SCRIPTDIR=$(realpath $(dirname ${BASH_SOURCE[0]}))
BASEDIR=${1}

cd ${BASEDIR}

if [[ ${ARCH} = "x86_64" ]] ; then
    NIMARCH=amd64
else
    NIMARCH=arm64
fi

if [[ ${OS} = "Darwin" ]] ; then
    # Not awesome, but this is what nim calls it.
    OS=macosx
else
    # We don't support anything else at the moment.
    OS=linux
fi

DEPS_DIR=${DEPS_DIR:-${BASEDIR}/files/deps}
DEP_LIB=${DEPS_DIR}/lib/${OS}-${NIMARCH}
DEP_SRC=${DEPS_DIR}/src
DEP_USR=${DEPS_DIR}/usr

# The paste doesn't work from stdin on MacOS, so leave this as is, please.
export OPENSSL_CONFIG_OPTS=$(echo "
enable-ec_nistp_64_gcc_128
no-afalgeng
no-apps
no-bf
no-camellia
no-cast
no-comp
no-deprecated
no-des
no-docs
no-dtls
no-dtls1
no-egd
no-engine
no-err
no-idea
no-md2
no-md4
no-mdc2
no-psk
no-quic
no-rc2
no-rc4
no-rc5
no-seed
no-shared
no-srp
no-ssl
no-tests
no-tls1
no-tls1_1
no-uplink
no-weak-ssl-ciphers
no-zlib
" | tr '\n' ' ')

MUSL_GCC_OUTPUT_LOC=${DEP_USR}/musl/bin/musl-gcc
MUSL_GCC_INSTALL_LOC=${DEP_USR}/musl/bin/musl-gcc.sh

mkdir -p ${DEP_LIB}

function color {
    case $1 in
        black)
            CODE=0
            ;;
        red)
            CODE=1
            ;;
        green)
            CODE=2
            ;;
        yellow)
            CODE=3
            ;;
        blue)
            CODE=4
            ;;
        magenta)
            CODE=5
            ;;
        cyan)
            CODE=6
            ;;
        *)
            CODE=7
            ;;
        esac

    printf "%s%s%s" "$(tput -T vt100 setaf "${CODE}")" "$2" "$(tput -T vt100 op)"
    #echo "$2"
}

function get_src {
  mkdir -p ${DEP_SRC}
  cd ${DEP_SRC}

  if [[ ! -d ${DEP_SRC}/${1} ]] ; then
    echo $(color cyan "Downloading ${1} from ${2}")
    git clone ${2}
  fi
  if [[ ! -d ${1} ]] ; then
    echo "Could not create directory: ${DEP_SRC}/${1}"
    exit 1
  fi
  cd ${1}
}

function ensure_musl {
  if [[ ${OS} = "macosx" ]] ; then
      return
  fi
  if [[ ! -f ${MUSL_GCC_INSTALL_LOC} ]] ; then
    get_src musl git://git.musl-libc.org/musl
    echo $(color cyan "Building musl")
    unset CC
    mkdir -p ${DEP_USR}
    ./configure --disable-shared --prefix=${DEP_USR}/musl/
    make clean
    make
    make install
    mv lib/*.a ${DEP_LIB}

    if [[ -f ${MUSL_GCC_OUTPUT_LOC} ]] ; then
      echo $(color green "Installed musl wrapper to " ${MUSL_GCC_INSTALL_LOC})
    else
      echo $(color red "Installation of musl failed!")
      exit 1
    fi
  fi
  mv ${MUSL_GCC_OUTPUT_LOC} ${MUSL_GCC_INSTALL_LOC}
  export CC=${MUSL_GCC_INSTALL_LOC}
}

function install_kernel_headers {
    if [[ ${OS} = "macosx" ]] ; then
      return
    fi
    echo $(color cyan "Installing kernel headers needed for musl install")
    get_src kernel-headers https://github.com/sabotage-linux/kernel-headers.git
    make ARCH=${ARCH} prefix=/musl DESTDIR=${DEP_USR} install
}

function ensure_openssl {
  if [[ ! -f ${DEP_LIB}/libssl.a ]] || [[ ! -f ${DEP_LIB}/libcrypto.a ]] ; then
      ensure_musl
      install_kernel_headers

      get_src openssl https://github.com/openssl/openssl.git
      echo $(color cyan "Building openssl")
      if [[ ${OS} == "macosx" ]]; then
          ./config ${OPENSSL_CONFIG_OPTS}
      else
          ./config ${OPENSSL_CONFIG_OPTS} -static
      fi
      make clean
      make build_libs
      mv *.a ${DEP_LIB}
      if [[ -f ${DEP_LIB}/libssl.a ]] && [[ -f ${DEP_LIB}/libcrypto.a ]] ; then
        echo $(color green "Installed openssl libs to ${DEP_LIB}")
      else
        echo $(color red "Installation of openssl failed!")
        exit 1
      fi
  fi
}

function ensure_pcre {
  if [[ ! -f ${DEP_LIB}/libpcre.a ]] ; then

    get_src pcre https://github.com/luvit/pcre.git
    echo $(color cyan "Building libpcre")
    # For some reason, build fails on arm if we try to compile w/ musl?
    unset CC
    ./configure --disable-cpp --disable-shared
    make clean
    make

    mv .libs/libpcre.a ${DEP_LIB}
    if [[ -f ${DEP_LIB}/libpcre.a ]] ; then
      echo $(color green "Installed libpcre to ${DEP_LIB}")
    else
      echo $(color red "Installation of libprce failed. This may be due to missing build dependencies. Please make sure autoconf, m4 and perl are installed.")
      exit 1
    fi
  fi
}

function remove_src {
  # Don't nuke the src if CON4M_DEV is on.
  if [[ -d ${DEP_SRC} ]] ; then
    if [[ -z ${CON4M_DEV+woo} ]] ; then
      echo $(color cyan "Removing code (because CON4M_DEV is not set)")
      rm -rf ${DEP_SRC}
    else
      echo $(color cyan "Keeping source code (CON4M_DEV is set!)")
    fi
  fi
}

ensure_musl
ensure_openssl
ensure_pcre

echo $(color green "All dependencies installed.")
remove_src
