#!/usr/bin/env bash
#
# Format the source code.

# Install tools if necessary
declare -ra tools=('cabal-fmt' 'stylish-haskell' 'brittany')
for tool_name in "${tools[@]}"
do
    if ! stack exec which "$tool_name"
    then
        stack build --copy-compiler-tool "$tool_name"
    fi
done

# Find Haskell files
hs_files=()
while IFS=  read -r -d $'\0'
do
    hs_files+=("$REPLY")
done < <(find . -name '*.hs' -not -path '*/\.*' -print0)

# Run tools (in the correct order)
stack exec cabal-fmt -- --tabular -i numbat.cabal
stack exec stylish-haskell -- --inplace "${hs_files[@]}"
stack exec brittany -- --indent 4 --write-mode=inplace "${hs_files[@]}"