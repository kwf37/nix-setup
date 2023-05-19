#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

# 1. Copy the template config file
echo Copying configuration.nix.template
cp configuration.nix.template configuration.nix

# 2. Declare all variables to replace in template file
declare -A template_vars

template_vars[CONFIG_REPO_PATH]=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# 3. Replace all variables to create final configuration.nix file
echo Replacing variables in configuration.nix
for var in "${!template_vars[@]}"
do
	sed -i "s|<<$var>>|${template_vars[$var]}|g" "${template_vars[CONFIG_REPO_PATH]}/configuration.nix"

done

echo Done
