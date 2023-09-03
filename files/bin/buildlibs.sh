#!/bin/bash

# ensure there is no silent failures
set -eEu
set -o pipefail

ARCH=$(uname -m)
OS=$(uname -o)

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

DEPS_DIR=${DEPS_DIR:-${HOME}/.local/c0}

PKG_LIBS=${1}/lib/${OS}-${NIMARCH}
MY_LIBS=${DEPS_DIR}/libs
SRC_DIR=${DEPS_DIR}/src
MUSL_DIR=${DEPS_DIR}/musl
MUSL_GCC=${MUSL_DIR}/bin/musl-gcc

mkdir -p ${MY_LIBS}
cd ${DEPS_DIR}

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

function color {
    case $1 in
        black)   CODE=0 ;;
        red)     CODE=1 ;; RED)     CODE=9 ;;
        green)   CODE=2 ;; GREEN)   CODE=10 ;;
        yellow)  CODE=3 ;; YELLOW)  CODE=11 ;;
        blue)    CODE=4 ;; BLUE)    CODE=12 ;;
        magenta) CODE=5 ;; MAGENTA) CODE=13 ;;
        cyan)    CODE=6 ;; CYAN)    CODE=14 ;;
        white)   CODE=7 ;; WHITE)   CODE=15 ;;
        grey)    CODE=8 ;; *)       CODE=$1 ;;
    esac
    shift

    echo -n $(tput setaf ${CODE})$@$(tput op)
}

function colorln {
    echo $(color $@)
}

function copy_from_package {
    for item in ${@}
    do
        if [[ ! -f ${MY_LIBS}/${item} ]] ; then
            if [[ ! -f ${PKG_LIBS}/${item} ]] ; then
                return 1
            else
                cp ${PKG_LIBS}/${item} ${MY_LIBS}
            fi
        fi
    done
    return 0
}

function get_src {
  mkdir -p ${SRC_DIR}
  cd ${SRC_DIR}

  if [[ ! -d ${SRC_DIR}/${1} ]] ; then
    echo $(color CYAN Downloading ${1} from:) ${2}
    git clone ${2}
  fi
  if [[ ! -d ${1} ]] ; then
    echo $(color RED Could not create directory: ) ${SRC_DIR}/${1}
    exit 1
  fi
  cd ${1}
}

function ensure_musl {
  if [[ ${OS} = "macosx" ]] ; then
      return
  fi
  if [[ ! -f ${MUSL_GCC} ]] ; then
    get_src musl git://git.musl-libc.org/musl
    colorln CYAN Building musl
    unset CC
    ./configure --disable-shared --prefix=${MUSL_DIR}
    make clean
    make
    make install
    mv lib/*.a ${MY_LIBS}

    if [[ -f ${MUSL_GCC} ]] ; then
      echo $(color GREEN Installed musl wrapper to:) ${MUSL_GCC}
    else
      colorln RED Installation of musl failed!
      exit 1
    fi
  fi
  export CC=${MUSL_GCC}
  export CXX=${MUSL_GCC}
}

function install_kernel_headers {
    if [[ ${OS} = "macosx" ]] ; then
      return
    fi
    colorln CYAN Installing kernel headers needed for musl install
    get_src kernel-headers https://github.com/sabotage-linux/kernel-headers.git
    make ARCH=${ARCH} prefix= DESTDIR=${MUSL_DIR} install
}

function ensure_openssl {

  if ! copy_from_package libssl.a libcrypto.a ; then
      ensure_musl
      install_kernel_headers

      get_src openssl https://github.com/openssl/openssl.git
      colorln CYAN Building openssl
      if [[ ${OS} == "macosx" ]]; then
          ./config ${OPENSSL_CONFIG_OPTS}
      else
          ./config ${OPENSSL_CONFIG_OPTS} -static
      fi
      make clean
      make build_libs
      mv *.a ${MY_LIBS}
      if [[ -f ${MY_LIBS}/libssl.a ]] && [[ -f ${MY_LIBS}/libcrypto.a ]] ; then
        echo $(color GREEN Installed openssl libs to:) ${MY_LIBS}
      else
        colorln RED Installation of openssl failed!
        exit 1
      fi
  fi
}

function ensure_pcre {
  if ! copy_from_package libpcre.a ; then

    get_src pcre https://github.com/luvit/pcre.git
    colorln CYAN "Building libpcre"
    # For some reason, build fails on arm if we try to compile w/ musl?
    unset CC
    ./configure --disable-cpp --disable-shared
    make clean
    make

    mv .libs/libpcre.a ${MY_LIBS}
    if [[ -f ${MY_LIBS}/libpcre.a ]] ; then
      echo $(color GREEN Installed libpcre to:) ${MY_LIBS}/libpcrea.
    else
      colorln RED "Installation of libprce failed. This may be due to missing build dependencies. Please make sure autoconf, m4 and perl are installed."
      exit 1
    fi
  fi
}

function remove_src {
  # Don't nuke the src if CON4M_DEV is on.
  if [[ -d ${SRC_DIR} ]] ; then
    if [[ -z ${CON4M_DEV+woo} ]] ; then
      colorln CYAN Removing code \(because CON4M_DEV is not set\)
      rm -rf ${SRC_DIR}
    else
      colorln CYAN Keeping source code \(CON4M_DEV is set\)
    fi
  fi
}

ensure_musl
ensure_openssl
ensure_pcre

colorln GREEN All dependencies satisfied.
remove_src
