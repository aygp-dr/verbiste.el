#!/usr/bin/env python3
import ollama
import sys

def test_ollama():
    try:
        # Test model info
        models = ollama.list()
        print(f"Available models: {models}")
        
        # Test embedding
        result = ollama.embeddings(model="nomic-embed-text", prompt="test")
        if "embedding" in result:
            print(f"Embedding test successful. Dimension: {len(result['embedding'])}")
            print(f"First 5 values: {result['embedding'][:5]}")
            return True
        else:
            print("Failed to get embedding")
            return False
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    print("Testing Ollama...")
    success = test_ollama()
    sys.exit(0 if success else 1)
