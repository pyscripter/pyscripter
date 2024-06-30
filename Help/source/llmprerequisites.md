:::{index} LLM Prerequisites
:::

# LLM Integration - Prerequisites

## OpenAI

You need to register with OpenAI and [create an API key](https://platform.openai.com/api-keys).
The API key can be either a project key or a legacy user key.  Please note that this is a paid 
service, so you need to either:
- get a payment plan or
- add credit on a pay-as-you-go basis

A $10 credit can get a long way when using OpenAI with PyScripter.  See also the 
[OpenAI pricing](https://openai.com/api/pricing/).  Note that the older **GPT-3.5 Turbo** 
model is 10 times cheaper than the newer **GPT-4o model**, which is also much cheaper than its 
predecesors **gpt-4-turbo** and **gpt-4**.

## Local Models using Ollama

Why you may want to run LLMs locally? Here are a few reasons:

- Great choice of open source LLM models
- Save money since it is free
- You may want to dive deeper and play with model parameters such as temperature and penalty.
- Train models with your or your company's data.

To use Ollama you need:

- a fairly modern and fast CPU.
- a modern and powerful GPU with at least 6 GB of memory. For intance, a relatively cheap Nvidia
  GeForce RTX 3060 with 12GB memory can achieve good performance.
- Quite a few GBs of free fast disk space.

Otherwise the user experience can be frustratingly slow.  

### Install Ollama

You first need to download the [ollama installer](https://ollama.com/download/OllamaSetup.exe) 
and install it. Note that after the installation, an Ollama Windows service will start automatically every time you start Windows.

### Install Ollama models

Ollama provides access to a large number of [LLM models](https://ollama.com/library) 
such as codegemma from Google and codelllama from Meta.  To use a given 
model you need to install it locally.  You can do that from a command prompt by 
issuing the command:

```
ollama pull model_name
```
After that you are ready to use the local model with PyScripter.

The [Assistant](assistant) currently works with the **codellama:code** model and its variants. 
You can use models such as **codellama**, **codegemma**, **starcoder2** and **deepseek-coder-v2**
with the [Chat](chatwindow). For each model you can pick the largest model size that can fit 
comfortably in you GPU memory.  For example, **codellama:13b-code-q3_K_M** is a 
**codellama:code** variant with size 6.3 GB that can be used with a GPU with 
8GB or more memory.  Bear in mind, that the larger the model size, the slower the model response will be, but the higher the quality of the answers.

