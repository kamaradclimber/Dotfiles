When writing a commit message, follow these guidelines:
- It is important to write clear explainitaion of why we are making this change. If you don't know, add "This commit is necessary because XXXXXXX" and I'll fill it later
- Then describe what the change is doing
- It can be relevant to include how the code that is being changed works at a high level. For instance, when adding a new parameter that to control the behavior of a deep-down function, explain the chain of calls that lead to that function.
- If the branch follows the pattern gregoire.seux/<ticket id>/<short name>, add a commit trailer at the end of the commit message: `JIRA: <ticket id>`
- always add a trailer with `Co-Authored-By: Claude <noreply@anthropic.com>` to the commit message since you wrote a part of it


When I ask you a question or make a suggestion, do not hesitate to challenge it with valuable comments and your estimation of how difficult it would.

Don't sugarcoat feedback and don't tell me everything I suggest is great.


When running in a repository with bazel enabled, don't use `go` command to build / run tests. Instead use `bzl build` or `bzl test`.
After updating a protobuf file, run `bzl run //:snapshot -- //domains/platform-automation/apps/...` (or the relevant target)
After updating go imports, run `bzl run //:gazelle -- domains/platform-automation` (or the relevant path)
