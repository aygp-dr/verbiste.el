#!/usr/bin/env python3
"""
Simple test script to generate an embedding for a single French word
using Sentence Transformers.
"""

from sentence_transformers import SentenceTransformer
import sys
import time
import json

def test_embedding(word="avoir"):
    """Generate embedding for a single word and display results"""
    
    print(f"Testing embedding generation for word: '{word}'")
    
    # Load model - use a multilingual model that's good for French
    print("Loading model...")
    start_time = time.time()
    model = SentenceTransformer('paraphrase-multilingual-MiniLM-L12-v2')
    load_time = time.time() - start_time
    print(f"Model loaded in {load_time:.2f} seconds")
    
    # Generate embedding
    print("Generating embedding...")
    start_time = time.time()
    embedding = model.encode(word)
    embed_time = time.time() - start_time
    
    # Print results
    print(f"Embedding generated in {embed_time:.2f} seconds")
    print(f"Embedding dimension: {len(embedding)}")
    print(f"First 10 values: {embedding[:10].tolist()}")
    
    # Return the embedding
    return embedding.tolist()

if __name__ == "__main__":
    # Get word from command line if provided
    word = sys.argv[1] if len(sys.argv) > 1 else "avoir"
    
    # Run test
    embedding = test_embedding(word)
    
    # Save to file
    output_file = f"{word}_embedding.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump({word: embedding}, f, ensure_ascii=False, indent=2)
    
    print(f"Embedding saved to {output_file}")
