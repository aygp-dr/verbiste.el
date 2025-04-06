#!/usr/bin/env python3
"""
Simple test script for TF-IDF embeddings.
"""

import sys
import json
from sklearn.feature_extraction.text import TfidfVectorizer

def test_tfidf_embedding(word="avoir"):
    """Generate an embedding for a single word using TF-IDF"""
    print(f"Testing TF-IDF embedding for word: '{word}'")
    
    # Create a TF-IDF vectorizer
    vectorizer = TfidfVectorizer(
        analyzer='char', 
        ngram_range=(2, 4)  # Character n-grams of size 2-4
    )
    
    # Fit and transform to get embedding
    X = vectorizer.fit_transform([word])
    
    # Get feature names (n-grams)
    feature_names = vectorizer.get_feature_names_out()
    embedding = X.toarray()[0]
    
    # Print results
    print(f"Embedding dimension: {len(embedding)}")
    print(f"Feature names (n-grams): {feature_names.tolist()}")
    print(f"Values: {embedding.tolist()}")
    
    return embedding.tolist()

if __name__ == "__main__":
    # Get word from command line if provided
    word = sys.argv[1] if len(sys.argv) > 1 else "avoir"
    
    # Run test
    embedding = test_tfidf_embedding(word)
    
    # Save to file
    result = {word: embedding}
    output_file = f"{word}_tfidf_embedding.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(result, f, ensure_ascii=False, indent=2)
    
    print(f"Embedding saved to {output_file}")
