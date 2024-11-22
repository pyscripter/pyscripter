:::{index} Assistant
:::

# The Assistant

You can access the Assistant from the context menu of the editor.

![graphic](images/assistantmenu.png){align=center width="28.19em" height="10.81em"}

*Suggest*\
Request from the Assistant a suggestion for code completion.   This is only available when there
is no selection in the editor.

The following three commands are only available when a part of the code is selected.

*Explain*\
Ask the Assistant to add comments explaining the selected piece of code.  The comments should help
users to understand the logic and functionality of the code.

*Fix bugs*\
Ask the Assistant to fix the bugs in the selected piece of code.  The Assistant is also 
expected to explain the nature of the original issues and how they were resolved, using comments
in the code.

*Optimize*\
Ask the Assistant to fix optimize the selected piece of code. The optimizations
should maintain the original functionality of the code.

*Cancel*\
Cancel a pending request. In all the above cases an activity indicator will appear in the
PyScripter Status Bar.  You can also cancel a request by clicking on the activity 
indicator.

*Settings*\
Shows and allows you to modify the Chat settings (see below).

## Assistant settings

Assistant settings are automatically saved when PyScripter exits and restored when 
PyScripter starts, in a Json file named "Assistant Settings.json" in the same directory 
as PyScripter.ini.

The settings are similar to the [Chat Settings](chatwindow.md#chat-settings) with the following
differences:

*Endpoint*\
The base URL for accessing the LLM API.  You shouldn't need to change the provided
defaults (which for OpenAI and Olama are different from chat): 
- https://api.openai.com/v1/completions for OpenAI
- http://localhost:11434/api/generate for Ollama

*Model*\
The model you want to use. Currently the Assistant works only with the following models:
- OpenAI model:\
 **gpt-3.5-turbo-instruct**\
   The reason is that this is the only OpenAI model that has been trained for 
  [Fill-in-the-middle](https://codeium.com/blog/why-code-completion-needs-fill-in-the-middle) 
  (FIM) tasks. And FIM is important for code completion.
- Gemini model:\
  **gemini-1.5-flash**, **gemini-1.5-pro** or any newer Gemini model
- Ollama model:\
  **codellama:code** and its variants (e.g. **codellama:7b-code-q6_K** will work).

Wider choice of Assistant models will be provided in the future.


## The Suggestion Window

All Assistant responses will appear inside a pop-up Suggestion Window:

![graphic](images/suggestionwindow.png){align=center width="35.25em" height="12.25em"}

You may edit the response before issuing one of the following commands

**Commands and shortcuts:**

*Accept (Tab)*\
Insert the contents of the suggestion at the cursor position.

*Accept Word (Ctrl+Right)*\
Accept only the first word of the suggestion.

*Accept Line (Ctrl+Enter)*\
Accept only the first line of the suggestion.

*Cancel (Esc)*\
Ignore the suggestion and return to the editor.


---

You can see video demos of using the PyScripter Assistant at this 
[blog post](https://pyscripter.blogspot.com/2024/06/teaser-integration-with-llm.html).

