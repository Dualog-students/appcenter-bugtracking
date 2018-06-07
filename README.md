# AppCenter bug tracking
This repo is just a sample of a serverless utility for processing webhooks used in our main organization.

## Bug tracking for Apps in [App Center](https://appcenter.ms)
Bug triage is a process where tracker issues are screened and prioritised. 
Triage should help ensure we appropriately manage all reported issues - bugs as well as issues.  
Triage initially happens shortly after the issue was reported but it can be repeated later if the initial assumptions were wrong, issue was resolved otherwise, affected versions need updating or there are other reasons to review the issue.

## Azure Functions (serverless-stateless applications)
Azure Functions is a solution for easily running small pieces of code, or "functions," in the cloud. One can write just the code that is needed for the problem at hand, without worrying about a whole application or the infrastructure to run it. 
Functions can make development even more productive, and one can use a development language of choice, such as C#, F#, Node.js, etc.

Azure Functions (like AWS lambdas) is a great way to run small processing tasks such as the one in `AzureFunction.fs` which processes the body of the issue and labels it with the right label. Without having to deply anything but a simple snippet of code, awesome!

