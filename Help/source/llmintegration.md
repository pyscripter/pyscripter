:::{index} Large Language Models, LLM
:::

# Large Language Models

[Large Language Models](https://en.wikipedia.org/wiki/Large_language_model) (LLMs) have the
potential of transforming the coding experience and increasing programmer productivity
to new levels.  PyScripter has built-in support for LLM-assisted coding, which comes
in two forms.

1. A coding [***Assistant***](assistant) available in the Editor
2. Integrated [***Chat***](chatwindow) for interacting with LLM models

Both cloud-based and local LLM are supported:

- Cloud-based LLM
  - [OpenAI](https://openai.com/) models such as (GPT-3.5 Turbo and GPT-4o)
- Local LLM models (support is provided by using [Ollama](https://github.com/ollama/ollama)).
  Choice of [models](https://ollama.com/library) include:
  - llama3 and codellama by Meta
  - gemma by Google
  - starcoder2 by Nvidia
  - phi and wizardlm by Microsoft
  - and many many others

Support for other cloud-based LLM services such as [Gemini](https://gemini.google.com/) by Google and [Claude](https://www.anthropic.com/claude) by Anthropic is planned in future
versions of PyScripter

:::{toctree}
llmprerequisites
assistant
chatwindow
:::



