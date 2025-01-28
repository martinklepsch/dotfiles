function load-ai-keys
    # Check if op CLI is installed
    if not command -v op &>/dev/null
        echo "Error: 1Password CLI (op) is not installed"
        echo "Please install it from: https://1password.com/downloads/command-line/"
        return 1
    end

    # Check if we're signed in to 1Password
    if not op account get &>/dev/null
        echo "Error: Not signed in to 1Password"
        echo "Please sign in using: op signin"
        return 1
    end

    # Define keys and their corresponding 1Password item references
    # CUSTOMIZE THESE VALUES:
    set key_mappings \
        "DEEPSEEK_API_KEY::op://Private/Deepseek LLM CLI/credential" \
        "ANTHROPIC_API_KEY::op://Private/Anthropic API key for CLI Usage/credential"

    set success_count 0
    set total_keys 0

    for mapping in $key_mappings
        set total_keys (math $total_keys + 1)
        set key_parts (string split "::" $mapping)
        set env_var_name $key_parts[1]
        set op_reference $key_parts[2]


        echo "Fetching $env_var_name from $op_reference"
        
        # Try to get the value from 1Password
        set value (op read "$op_reference" 2>/dev/null)
        
        if test $status -eq 0 -a -n "$value"
            set -gx $env_var_name $value
            echo "✅ Successfully set $env_var_name"
            set success_count (math $success_count + 1)
        else
            echo "❌ Failed to retrieve $env_var_name"
        end
    end
end
