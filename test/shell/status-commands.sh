# Run suggestions in the shell they are printed for.
run_suggestion() {
    case "$(uname -s)" in
        MINGW*|MSYS*) MSYS2_ARG_CONV_EXCL='*' powershell.exe -NoProfile -NonInteractive -Command "$1; if (!\$?) { exit 1 }" ;;
        *) sh -ec "$1" ;;
    esac
}

next_command() {
    hledger-flow status "$@" --json | jq -er '.next.command' | tr -d '\r'
}

# Match the native path reported by the executable, including Windows drive letters.
native_pwd() {
    case "$(uname -s)" in
        MINGW*|MSYS*) cygpath -aw "$(pwd -P)" ;;
        *) pwd -P ;;
    esac
}
