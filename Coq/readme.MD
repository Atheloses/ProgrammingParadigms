# The Coq Proof Assistant
## Windows - Installing Coq
### Install Chocolatey with PowerShell
* First check if `Get-ExecutionPolicy` returns Restricted
* If so then run `Set-ExecutionPolicy AllSigned` or `Set-ExecutionPolicy Bypass -Scope Process`
* Finally install Choco with `Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))`

### Install Coq
*   `chocolatey install coq -y`
*   Check if path to Coq is set in Environment variable Path
* At this point you should be able to use CoqIde software

### Install Visual Studio Code
* https://code.visualstudio.com/Download

### Install VSCoq extension
* https://marketplace.visualstudio.com/items?itemName=maximedenes.vscoq

## Coq introduction
* Coq is a proof assistant. It means that it is designed to develop mathematical proofs, and especially to write formal specifications, programs and proofs that programs comply to their specifications. An interesting additional feature of Coq is that it can automatically extract executable programs from specifications, as either Objective Caml or Haskell source code.
* Properties, programs and proofs are formalized in the same language called the Calculus of Inductive Constructions (CIC). Then, all logical judgments in Coq are typing judgments: the very heart of Coq is in fact a type-checking algorithm.
