#!/usr/bin/env python3
"""
Simple test script for Google Generative AI embeddings.
"""

import os
from dotenv import load_dotenv
from google import genai
from google.genai import types
import json

def test_batch_embeddings():
    # Sample French verbs
    words = [
        "avoir", "être", "aller", "faire", "dire",
        "voir", "prendre", "venir", "pouvoir", "vouloir"
    ]
    
    # Load API key from .env file
    load_dotenv()
    api_key = os.getenv("GOOGLE_API_KEY")
    
    if not api_key:
        print("Error: GOOGLE_API_KEY not found in .env file")
        return None
    
    # Initialize client
    client = genai.GenerativeModel(model_name="text-embedding-004", 
                                  generation_config={"temperature": 0},
                                  api_key=api_key)
    
    # Generate embeddings
    print(f"Generating embeddings for {len(words)} words...")
    
    response = client.models.embed_content(
        model='models/text-embedding-004',
        contents=words,
        config=types.EmbedContentConfig(task_type='semantic_similarity')
    )
    
    # Extract embeddings from response
    embeddings = response.embeddings
    
    # Create dictionary of word -> embedding
    results = {}
    for i, word in enumerate(words):
        results[word] = embeddings[i]
    
    return results

if __name__ == "__main__":
    # Run test
    embeddings = test_batch_embeddings()
    
    # Save to file if embeddings were generated
    if embeddings:
        output_file = "french_verbs_google_embeddings.json"
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(embeddings, f, ensure_ascii=False, indent=2)
        
        print(f"Embeddings saved to {output_file}")
