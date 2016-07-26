#!/usr/bin/env bash

set -eu -o pipefail

osx() { [[ "$TRAVIS_OS_NAME" == "osx" ]]; }
linux() { [[ "$TRAVIS_OS_NAME" == "linux" ]]; }
if osx; then brew update && brew install gcc5; fi

if [ -e "$HOME/otp/$OTP_RELEASE/activate" ]; then exit 0; fi

mkdir -p ~/otp
cd ~/otp

if linux; then
    wget https://s3.amazonaws.com/travis-otp-releases/binaries/$(lsb_release -is | tr "A-Z" "a-z")/$(lsb_release -rs)/$(uname -m)/erlang-${OTP_RELEASE}-nonroot.tar.bz2
    tar xjf erlang-${OTP_RELEASE}-nonroot.tar.bz2
fi

if osx; then
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
    chmod a+x kerl
    travis_wait 60 ./kerl build $OTP_RELEASE $OTP_RELEASE
    ./kerl install $OTP_RELEASE ~/otp/$OTP_RELEASE
fi
