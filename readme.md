# `emacs-mcp`

## Usage

```sh
git clone -r git@github.com:dotemacs/emacs-mcp
```

And then:

```sh
$ echo '{ "jsonrpc": "2.0", "method": "tools/call", "params": \
{ "name": "execute_elisp", "arguments": { "code": "(emacs-uptime)" } }, "id": 1 }' | ./emacs-mcp.sh
```
