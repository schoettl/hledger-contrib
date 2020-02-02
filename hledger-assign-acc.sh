#!/bin/bash
# Interactively assign accounts from a list to hledger book entries.
# By default does not change the existing ledger file.
# fzf must be installed.

printUsage() {
    cat <<EOF
usage: $PROGNAME [options] -u UNASSIGNED_ACCOUNT
       $PROGNAME -h

If neither -a nor -A are given, the list of accounts is determined by
'hledger accounts'.

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
  -i
     update the original file in-place. dangerous! use this option only if you
     have your ledger files under version control.
  -p
     pretty-print the output ledgerfile. warning: it uses 'hledger print' which
     discards all hledger statements (like include, commodity, etc.).
  -h
     print help message.
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
    while getopts 'f:u:a:A:o:pih' option; do
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
            i)  declare -gr IN_PLACE=1
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

    [[ -n $ACCOUNTS_FILE && -n $ACCOUNT ]] \
        && exitWithError "error: you cannot specify both -a or -A."

    [[ -n $OUTPUT_FILE && -n $IN_PLACE ]] \
        && exitWithError "error: you cannot specify both -o and -i."

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
    if [[ -n $IN_PLACE ]]; then
        outputFile=$(mktemp)
        rm "$outputFile"
        # without rm, the error below would be triggered
    elif [[ -z $OUTPUT_FILE ]]; then
        outputFile=$ledgerFile.assigned
    else
        outputFile=$OUTPUT_FILE
    fi
    [[ -e $outputFile ]] \
        && exitWithError "error: output file exists already: $outputFile"

    if [[ -z $ACCOUNTS_FILE && -z $ACCOUNT ]]; then
        declare ACCOUNTS_FILE
        ACCOUNTS_FILE=$(mktemp)
        hledger -f "$ledgerFile" accounts > "$ACCOUNTS_FILE"
    fi

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
                newAccount=$(fzfSelectAccount)
                if [[ -z $newAccount ]]; then
                    echo "You did not select an account. The posting will not be changed."
                    echo
                    newAccount=$UNASSIGNED_ACCOUNT
                fi
            fi
            # Substitute first occurance of UNASSIGNED_ACCOUNT with newAccount
            awk \
              -v x="$UNASSIGNED_ACCOUNT" \
              -v y="$newAccount" \
              '{sub(x, y); print}' <<<"$line" \
              | echoAndAppend "$outputFile"
        else
            echo "$line" \
              | echoAndAppend "$outputFile"
        fi
    done < "$ledgerFile"

    if [[ -n $PRETTY_PRINT ]]; then
        declare tmp
        tmp=$(mktemp)
        hledger -f "$outputFile" print > "$tmp" && mv "$tmp" "$outputFile"
    fi

    if [[ -n $IN_PLACE ]]; then
        cp "$outputFile" "$ledgerFile"
        echo
        echo "Your ledger file has been modified."
        echo "Check all the changes with your version control's diff view, e.g. 'git diff'."
    fi
}

echoAndAppend() {
    declare outputFile=$1
    tee -a "$outputFile"
}

fzfSelectAccount() {
    (
        # unset errexit in only subshell!
        set +e
        # see man fzf /EXIT STATUS
        fzf --reverse --height=60%< "$ACCOUNTS_FILE" | cut -d$'\t' -f1
        if (( $? == 0 || $? == 1 )); then
            return 0
        fi
        return 1
    )
}

main "$@"
