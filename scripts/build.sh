#!/usr/bin/env bash

set -o errexit

APP_NAME="go-haskell"

BUILD_DIR="dist-newstyle"

SCRIPT_DIR="$(cd `dirname ${0}` && pwd)"

DIST_DIR="${SCRIPT_DIR}/../dist"

cabal build --enable-benchmarks all

BUILD_ARTIFACT_PATH="$(find ${BUILD_DIR} -name ${APP_NAME} -type f)"

mkdir -p "${DIST_DIR}"

cp -r "${BUILD_ARTIFACT_PATH}" "${DIST_DIR}/${APP_NAME}" 
