:::{index} IDE Windows; Chat
:::
:::{index} LLM, Chat Window
:::

# The Chat Window

The Chat window serves the purpose of interacting with Large Language Models (LLMs)
withoug leaving PyScripter.  It features the following:

- Choice of LLMs
- Both cloud-based and local LLM are supported
- The chat is organized around multiple topics
- Each topic can have its own title
- Can save and restore the chat history and settings
- Python code is syntax highlighted
- Python code can be easily copied to a code editor
- Within each topic the conversation context is maintained
- Spell-checking the question/prompt

![graphic](images/chatwindow.png){align=center width="32em" height="30.68em"}

**Toolbar Commands:**

*New Topic*\
Adds a new chat topic.

*Remove current topic*\
Remove the current chat topic.

*Save Chat history*\
Save chat topics to a Json file called "Chat history.json" in the same directory as 
PyScripter.ini.  The Chat history is automatically restored whe PyScripter starts.

*Title*\
Provide a title to the current topic.  The topic title is displayed in the window title.

*Next/Previous Topic*\
Show the next/previous topic.

*Settings*\
Shows and allows you to modify the Chat settings (see below).


## Chat Settings

Chat settings are automatically saved when PyScripter exits and restored when 
PyScripter starts, in a Json file named "Chat Settings.json" in the same directory 
as PyScripter.ini.

![graphic](images/chatsettings.png){align=center width="24.06em" height="15.87em"}

The following settings can be modified:

*Openai/Gemini/Ollama*\
Choose whether you want to use cloud-based OpenAI/Gemini models or local Ollama models.

*Endpoint*\
The base URL for accessing the LLM API.  You shouldn't need to change the provided
defaults: 
- https://api.openai.com/v1/chat/completions for OpenAI
- https://generativelanguage.googleapis.com/v1beta for Gemini
- http://localhost:11434/api/chat for Ollama

*Model*\
The model you want to use.   
- OpenAI models\
  **GPT-3.5 Turbo** is good for coding tasks.  **GPT-4o model** is newer, ten times more expensive, but possibly better.
- Gemini models\
  Use **gemini-1.5-flash** (faster and cheeper) or **gemini-1.5-pro** or any newer Gemini model.
- Ollama\
  You have a wide choice of good models. See the 
  [instructions](llmprerequisites.md#install-ollama-models) for chosing and installing 
  Ollama models.

*API key*\
Enter your OpenAI/Gemini [API key](llmprerequisites). Leave blank for Ollama models.

*Timeout*\
How long you are prepared to wait for a response in seconds.

*Maximum number of response tokens*\
An integer value that determines the maximum length of the response in number of tokens which
roughly correspond to words.

*System prompt* \
A string providing context to the LLM, e.g. "You are my expert python coding assistant".

## The Chat context menu

![graphic](images/chatcontextmenu.png){align=center width="13.69em" height="5.94em"}

**Commands:**

*Copy*\
Copies a the question or response under the cursor to the Clipboard.

*Copy Code*\
Copies the python code under the cursor to the Clipboard.

*Copy Code to New Editor*\
Copies the python code under the cursor to a new editor, so that you can 
readliy test it.

## Entering prompts

When you type a question/prompt you can use [parameters and modifiers](parameters).  For 
example you can use the following prompt instead of copying and pasting code from 
the editor

```
Please explain the following code:
$[SelText]
```

You can invoke parameter completion with Shift+Ctrl+P and modifier completion with
Shift+Ctrl+M.

You can also spell check the prompt by using the prompt context menu.

To submit your request, press *Ctrl+Enter* or click on the chat icon on the right of 
the question.

---

You can see video demos of using the PyScripter Chat at this 
[blog post](https://pyscripter.blogspot.com/2024/06/teaser-integration-with-llm.html).