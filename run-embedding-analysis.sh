#!/bin/sh
MODEL='nomic-embed-text'
# Ensure Ollama is running
ollama_status=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:11434/api/tags)

if [ "$ollama_status" != "200" ]; then
    echo "Ollama server is not running. Please start it with 'ollama serve'"
    exit 1
fi

# Check if models are available
if ! ollama list | grep -q "nomic-embed-text"; then
    echo "Pulling nomic-embed-text model..."
    ollama pull nomic-embed-text
fi

# Run the embedding analysis with both models
echo "Running analysis with nomic-embed-text..."
python verbiste_tools/embed_with_ollama.py --model nomic-embed-text --pca --save-data

echo "Done! Check french_verb_similarity.png and french_verb_similarity_pca.png for results."
