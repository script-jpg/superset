:- include('types.pro').
:- include('helpers.pro').
:- include('generation.pro').

% Use persisted data files
:- include('../data/persisted_data.pro'). % single file for all persisted facts
:- include('ollama_agent.pro'). % Ollama agent utilities
:- include('agent_actions.pro'). % Agent action execution module