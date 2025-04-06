#!/usr/bin/env python3
"""
Generate embeddings for French verbs using spaCy.
"""

import json
import os
import sys
from tqdm import tqdm
import spacy

def generate_verb_embeddings(input_file, output_file):
    """Generate embeddings for French verbs using spaCy and save to JSON."""
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file) or '.', exist_ok=True)
    
    # Read verb list
    with open(input_file, 'r', encoding='utf-8') as file:
        verbs = [line.strip() for line in file if line.strip()]
    
    print(f"Found {len(verbs)} verbs to process")
    
    # Load spaCy model
    print("Loading French language model...")
    nlp = spacy.load("fr_core_news_md")
    print("Model loaded successfully")
    
    # Check if output file already exists to resume progress
    embeddings = {}
    if os.path.exists(output_file):
        try:
            with open(output_file, 'r', encoding='utf-8') as file:
                embeddings = json.load(file)
            print(f"Loaded {len(embeddings)} existing embeddings from {output_file}")
            
            # Filter verbs that already have embeddings
            verbs = [verb for verb in verbs if verb not in embeddings]
            print(f"Remaining verbs to process: {len(verbs)}")
        except json.JSONDecodeError:
            print(f"Error loading existing embeddings from {output_file}, starting fresh")
    
    # Process verbs in batches
    batch_size = 100
    for i in range(0, len(verbs), batch_size):
        batch = verbs[i:i+batch_size]
        
        # Process batch
        print(f"Processing batch {i//batch_size + 1}/{(len(verbs) + batch_size - 1)//batch_size}...")
        for verb in tqdm(batch):
            try:
                # Generate embedding
                doc = nlp(verb)
                if doc.has_vector:
                    embeddings[verb] = doc.vector.tolist()
                else:
                    print(f"Warning: No vector found for '{verb}'")
            except Exception as e:
                print(f"Error processing '{verb}': {str(e)}")
        
        # Save progress after each batch
        with open(output_file, 'w', encoding='utf-8') as file:
            json.dump(embeddings, file, ensure_ascii=False)
        print(f"Saved progress: {len(embeddings)}/{len(verbs) + len(embeddings)} verbs processed")
    
    # Print summary
    print(f"Successfully created embeddings for {len(embeddings)} verbs")
    print(f"Saved to {os.path.abspath(output_file)}")
    
    # Show a sample embedding
    if embeddings:
        first_verb = next(iter(embeddings))
        print(f"\nSample embedding for '{first_verb}':")
        print(f"Dimension: {len(embeddings[first_verb])}")
        print(f"First 5 values: {embeddings[first_verb][:5]}")

if __name__ == "__main__":
    # Check arguments
    if len(sys.argv) != 3:
        print("Usage: python generate_verb_embeddings.py INPUT_FILE OUTPUT_FILE")
        sys.exit(1)
    
    # Get input and output files from command line arguments
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Generate embeddings
    generate_verb_embeddings(input_file, output_file)
