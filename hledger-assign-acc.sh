#!/bin/bash
# Interactively assign accounts from a list to hledger book entries.
# It never changes the existing ledger file!

# TODO smart detection of account names instead of accounts file
# TODO on empty selection, do not change the account!

printUsage() {
    cat <<EOF
usage: $PROGNAME [options]

options:
  -f LEDGER_FILE
     if not set, look for the environment variable LEDGER_FILE and if not set
     fall back to ~/.hledger.journal
  -u UNASSIGNED_ACCOUNT
     the unassigned account name, e.g. 'income:unknown'.
  -a ACCOUNTS_FILE
     use this file to interactivly lookup possible accounts with fzf.
     the format must be:
     1. one account per line,
     2. each line tab-separated,
     3. first column is the account name.
  -A ACCOUNT
     always use this account and assign it automatically.
  -o OUTPUT_FILE
     the output file. if not specified, output goes to LEDGER_FILE.assigned
  -p
     pretty-print the output ledgerfile
  -h
     print help message
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
    while getopts 'f:u:a:A:o:ph' option; do
        case $option in
            h)  printUsage
                exit 0
                ;;
            f)  declare -gr LEDGER_FILE=$OPTARG
                ;;
            u)  declare -gr UNASSIGNED_ACCOUNT=$OPTARG
                ;;
            a)  declare -gr ACCOUNTS_FILE=$OPTARG
                ;;
            A)  declare -gr ACCOUNT=$OPTARG
                ;;
            o)  declare -gr OUTPUT_FILE=$OPTARG
                ;;
            p)  declare -gr PRETTY_PRINT=1
                ;;
            *)  printUsage >&2
                # prints usage after the default error message (invalid option or missing option argument).
                # default error messages are disabled if the first character of optstring is ":".
                exit 1
                ;;
        esac
    done
    shift $((OPTIND-1))

    [[ -n $UNASSIGNED_ACCOUNT ]] \
        || exitWithError "error: -u must be specified."

    [[ -n $ACCOUNTS_FILE || -n $ACCOUNT ]] \
        || exitWithError "error: either -a or -A must be specified."

    [[ -n $ACCOUNTS_FILE && -n $ACCOUNT ]] \
        && exitWithError "error: either -a or -A must be specified."

    if (( $# != 0 )); then
        printUsage
        exit 1
    fi

    return 0
}


main() {
    parseCommandLine "$@"

    declare ledgerFile=${LEDGER_FILE:-~/.hledger.journal}
    declare outputFile
    if [[ -z $OUTPUT_FILE ]]; then
        outputFile=$ledgerFile.assigned
    else
        outputFile=$OUTPUT_FILE
    fi
    [[ -e $outputFile ]] \
        && exitWithError "error: output file exists already: $outputFile"

    while IFS= read -r line; do
        # TODO not good enough, may find too much lines:
        if grep -q '^ ' <<<"$line" && grep -qwF "$UNASSIGNED_ACCOUNT" <<<"$line"; then
            declare newAccount
            if [[ -n $ACCOUNT ]]; then
                # TODO for this case, we could also prepend and alias declaration, that would probably be safer
                newAccount=$ACCOUNT
            else
                # ACCOUNTS_FILE is defined
                # Highlight the unassigned account (instead of echo):
                grep -wF "$UNASSIGNED_ACCOUNT" <<<"${line/ /*}"
                echo
                newAccount=$(fzf --reverse --height=60%< "$ACCOUNTS_FILE" | cut -d$'\t' -f1)
                if [[ -z $newAccount ]]; then
                    # TODO ask if sure and repeat
                    newAccount=$UNASSIGNED_ACCOUNT
                fi
            fi
            # Substitute first occurance of UNASSIGNED_ACCOUNT with newAccount
            awk \
              -v x="$UNASSIGNED_ACCOUNT" \
              -v y="$newAccount" \
              '{sub(x, y); print}' <<<"$line" \
              | tee -a "$outputFile"
        else
            echo "$line" \
              | tee -a "$outputFile"
        fi
    done < "$ledgerFile"

    if [[ -n $PRETTY_PRINT ]]; then
        declare tmp
        tmp=$(mktemp)
        hledger -f "$outputFile" print > "$tmp" && mv "$tmp" "$outputFile"
    fi
}

main "$@"
