#!/usr/bin/env python3
"""
Simple test script for spaCy embeddings.
Tests word vector embeddings for French words using spaCy.
"""

import sys
import json
import os

def test_spacy_embedding(word="avoir"):
    """Generate an embedding for a single word using spaCy"""
    try:
        import spacy
    except ImportError:
        print("Error: spaCy is not installed. To install, run:")
        print("  poetry add spacy")
        print("  poetry run python -m spacy download fr_core_news_md")
        return None
    
    print(f"Testing spaCy embedding for word: '{word}'")
    
    # Try to load the French model
    try:
        print("Loading French language model...")
        nlp = spacy.load("fr_core_news_md")
        print("Model loaded successfully")
    except OSError:
        print("Error: French model not found. To install, run:")
        print("  poetry run python -m spacy download fr_core_news_md")
        return None
    
    # Process the word with spaCy
    print(f"Generating embedding for '{word}'...")
    doc = nlp(word)
    
    # Check if the word has a vector
    if doc.has_vector:
        # Get the word vector
        embedding = doc.vector
        
        # Print information about the embedding
        print(f"Embedding dimension: {len(embedding)}")
        print(f"Embedding norm: {doc.vector_norm:.4f}")
        print(f"First 5 values: {embedding[:5].tolist()}")
        
        return embedding.tolist()
    else:
        print(f"Warning: No vector found for '{word}'")
        return None

def compare_words(words):
    """Compare embeddings for multiple words"""
    if not words or len(words) < 2:
        return
    
    try:
        import spacy
        import numpy as np
    except ImportError:
        print("Required packages not installed")
        return
    
    try:
        nlp = spacy.load("fr_core_news_md")
    except OSError:
        print("French model not found")
        return
    
    print(f"\nComparing embeddings for: {', '.join(words)}")
    
    # Get embeddings for all words
    docs = [nlp(word) for word in words]
    
    # Calculate similarity matrix
    print("\nSimilarity matrix:")
    for i, doc1 in enumerate(docs):
        for j, doc2 in enumerate(docs):
            similarity = doc1.similarity(doc2)
            print(f"  {words[i]} <-> {words[j]}: {similarity:.4f}")
    
    # Find most similar pair
    most_similar = (0, 0, 0.0)
    for i in range(len(docs)):
        for j in range(i+1, len(docs)):
            similarity = docs[i].similarity(docs[j])
            if similarity > most_similar[2]:
                most_similar = (i, j, similarity)
    
    if most_similar[2] > 0:
        i, j, sim = most_similar
        print(f"\nMost similar pair: {words[i]} and {words[j]} with similarity {sim:.4f}")

if __name__ == "__main__":
    # Get word from command line if provided
    word = sys.argv[1] if len(sys.argv) > 1 else "avoir"
    
    # Run test for a single word
    embedding = test_spacy_embedding(word)
    
    # Save to file if embedding was generated
    if embedding:
        result = {word: embedding}
        output_file = f"{word}_spacy_embedding.json"
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(result, f, ensure_ascii=False, indent=2)
        
        print(f"Embedding saved to {output_file}")
        
        # Compare with some related words
        if word == "avoir":
            compare_words(["avoir", "être", "posséder", "tenir", "obtenir"])
        elif word == "manger":
            compare_words(["manger", "dévorer", "avaler", "consommer", "dîner"])
        else:
            # Try to compare with some related forms by adding common prefixes/suffixes
            related = [
                word,
                "re" + word if not word.startswith("re") else word[2:],
                "dé" + word if not word.startswith("dé") else word[2:],
                word + "er" if not word.endswith("er") else word[:-2],
                word + "ir" if not word.endswith("ir") else word[:-2]
            ]
            compare_words([w for w in related if w != word and len(w) > 2])
