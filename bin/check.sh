#!/usr/bin/env bash
set -Eeu

# Test if requirements are satisfied
function check_requirements {
  if ! command -v raco >> /dev/null 2>&1 || \
  ! command -v ./dist/bin/scryer-prolog >> /dev/null 2>&1
  then

    if ! command -v ./dist/bin/scryer-prolog >> /dev/null 2>&1
    then
      print_utf8 '%s.%s' 'Please install [scryer-prolog](https://github.com/mthom/scryer-prolog/blob/42a0d686a5db241924490a5cca5c8b1d5bd0e5b0/README.md#installing-scryer-prolog) executable' $'\n'
    fi

    if ! command -v raco >> /dev/null 2>&1;
    then
      print_utf8 '%s.%s' 'Please install [raco](https://download.racket-lang.org/)' $'\n'
    fi

    return 1
  fi
}

set +Eeu
