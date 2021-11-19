#!/bin/bash

printUsage() {
    cat <<EOF
$PROGNAME - Transform hledger's register and print output.

usage: $PROGNAME [options]

options:
  -t tab-separated instead of space-separated; useful for use in text
     processing software with tabstops.
     requirement: descriptions must not contain two consecutive whitespace!
  -c omit all comments
  -d DESCRIPTION_WIDTH
     shorten description to n characters.
     requires -t.
  -a ACCOUNT_WIDTH
     shorten account name to n characters.
     requires -t.
  -s a single date, i.e. "2021-11-19" instead of "2021-11-19=2021-11-20".
  -h print help message.
EOF
}

set -o errexit -o pipefail
shopt -s nullglob

readonly PROGNAME=${0##*/}

# $1: error message
exitWithError() {
    declare msg=${1:-}
    echo "$msg" >&2
    exit 1
}

# $*: command line arguments = "$@"
parseCommandLine() {

    declare option
    while getopts 'ha:d:cts' option; do
        case $option in
            h)  printUsage
                exit 0
                ;;
            d)  declare -gr DESCRIPTION_WIDTH=$OPTARG
                ;;
            a)  declare -gr ACCOUNT_WIDTH=$OPTARG
                ;;
            t)  declare -gr TAB_SEPARATED=1
                ;;
            c)  declare -gr NO_COMMENTS=1
                ;;
            s)  declare -gr SINGLE_DATE=1
                ;;
            *)  printUsage >&2
                # prints usage after the default error message (invalid option or missing option argument).
                # default error messages are disabled if the first character of optstring is ":".
                exit 1
                ;;
        esac
    done
    shift $((OPTIND-1))

    [[ -z $DESCRIPTION_WIDTH || $DESCRIPTION_WIDTH =~ [0-9]+ ]] \
        || exitWithError "error: DESCRIPTION_WIDTH must be integer."

    [[ -z $ACCOUNT_WIDTH || $ACCOUNT_WIDTH =~ [0-9]+ ]] \
        || exitWithError "error: ACCOUNT_WIDTH must be integer."

    [[ ( -n $DESCRIPTION_WIDTH || -n $ACCOUNT_WIDTH ) && -z $TAB_SEPARATED ]] \
        && exitWithError "error: shorten description or account only works with -t."

    if (( $# != 0 )); then
        printUsage
        exit 1
    fi

    return 0
}


main() {
    parseCommandLine "$@"

    singleDate | noComments | tabSeparated | shorten
}

singleDate() {
  if [[ -n $SINGLE_DATE ]]; then
    sed -r 's/^([0-9]{4}[^ =]*)=[0-9]{4}[^ ]+/\1/'
  else
    cat
  fi
}

noComments() {
  if [[ -n $NO_COMMENTS ]]; then
    sed -e 's/  *;.*//'
  else
    cat
  fi
}

tabSeparated() {
  if [[ -n $TAB_SEPARATED ]]; then
    sed -e '/^[^ ]/s/ /\t/' -e '/^   *[^ ]/s/   */\t\t/' -e 's/   */\t/g'
  else
    cat
  fi
}

shorten() {
  if [[ -n $DESCRIPTION_WIDTH || -n $ACCOUNT_WIDTH ]]; then
    awk -F$'\t' -vOFS=$'\t' -v dw="$DESCRIPTION_WIDTH" -v aw="$ACCOUNT_WIDTH" \
      'function shorten(s, n) {
          if (length(s) > n)
            return substr(s, 0, n-1) "…"
          return s
       }
       {
          if (dw) $2 = shorten($2, dw)
          if (aw) $3 = shorten($3, aw)
          print
       }'
  else
    cat
  fi
}

main "$@"
