#!/bin/bash
# Emacs MCP server implementation
# Provides Emacs integration via elisp evaluation

# Override configuration paths BEFORE sourcing the core
MCP_CONFIG_FILE="$(dirname "${BASH_SOURCE[0]}")/emacs_config.json"
MCP_TOOLS_LIST_FILE="$(dirname "${BASH_SOURCE[0]}")/emacs_tools_list.json"
MCP_LOG_FILE="$(dirname "${BASH_SOURCE[0]}")/emacs_mcp.log"

# MCP Server Tool Function Guidelines:
# 1. Name all tool functions with prefix "tool_" followed by the same name defined in tools_list.json
# 2. Function should accept a single parameter "$1" containing JSON arguments
# 3. For successful operations: Echo the expected result and return 0
# 4. For errors: Echo an error message and return 1
# 5. All tool functions are automatically exposed to the MCP server based on tools_list.json

# Source the core MCP server implementation from the submodule
source "$(dirname "${BASH_SOURCE[0]}")/mcp-core/mcpserver_core.sh"

# Check if emacsclient is available
if ! command -v emacsclient &> /dev/null; then
    log "ERROR" "emacsclient not found. Please ensure Emacs is installed with server mode enabled."
    exit 1
fi

# Helper function to safely execute elisp code
execute_elisp() {
    local elisp_code="$1"
    emacsclient -e "$elisp_code" 2>/dev/null
    return $?
}

# Tool: Get current buffer information
tool_get_buffer_info() {
    local result=$(execute_elisp '(let ((buf (current-buffer)))
        (json-encode
            `((name . ,(buffer-name buf))
              (file . ,(or (buffer-file-name buf) ""))
              (modified . ,(buffer-modified-p buf))
              (size . ,(buffer-size buf))
              (mode . ,(symbol-name major-mode))
              (readonly . ,(buffer-local-value buffer-read-only buf)))))')
    
    if [[ $? -ne 0 ]]; then
        log "ERROR" "Failed to get buffer information"
        echo "null"
        return 1
    fi
    
    echo "$result"
    return 0
}

# Tool: Execute elisp code
tool_execute_elisp() {
    local args="$1"
    local code=$(echo "$args" | jq -r '.code')
    
    if [[ -z "$code" ]]; then
        log "ERROR" "No elisp code provided"
        echo "null"
        return 1
    fi
    
    local result=$(execute_elisp "$code")
    
    if [[ $? -ne 0 ]]; then
        log "ERROR" "Failed to execute elisp code: $code"
        echo "null"
        return 1
    fi
    
    # Format the result as JSON
    echo "{\"result\": $(jq -R -s '.' <<< "$result")}"
    return 0
}

# Tool: Search in current buffer
tool_search_buffer() {
    local args="$1"
    local pattern=$(echo "$args" | jq -r '.pattern')
    
    if [[ -z "$pattern" ]]; then
        log "ERROR" "No search pattern provided"
        echo "null"
        return 1
    fi
    
    # Escape the pattern for elisp
    pattern="${pattern//\\/\\\\}"
    pattern="${pattern//\"/\\\"}"
    
    local elisp_code="(let ((results nil) (case-fold-search t))
        (save-excursion
            (goto-char (point-min))
            (while (re-search-forward \"$pattern\" nil t)
                (let* ((match-start (match-beginning 0))
                       (match-end (match-end 0))
                       (line-num (line-number-at-pos match-start))
                       (line-text (buffer-substring-no-properties 
                                  (line-beginning-position) 
                                  (line-end-position))))
                    (push (list :line line-num :text line-text) results))))
        (json-encode (nreverse results)))"
    
    local result=$(execute_elisp "$elisp_code")
    
    if [[ $? -ne 0 ]]; then
        log "ERROR" "Failed to search buffer for pattern: $pattern"
        echo "null"
        return 1
    fi
    
    echo "$result"
    return 0
}

# Start the MCP server
run_mcp_server "$@"
