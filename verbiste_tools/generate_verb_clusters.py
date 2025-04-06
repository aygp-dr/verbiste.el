#!/usr/bin/env python3
"""
Generate clusters of similar French verbs based on embedding similarity.
"""

import json
import os
import sys
from tqdm import tqdm
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

def generate_verb_clusters(embeddings_file, output_file, threshold=0.65, max_similar=10):
    """Generate clusters of similar French verbs based on embedding similarity."""
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file) or '.', exist_ok=True)
    
    # Load verb embeddings
    print(f"Loading embeddings from {embeddings_file}")
    with open(embeddings_file, 'r', encoding='utf-8') as file:
        embeddings = json.load(file)
    
    print(f"Loaded embeddings for {len(embeddings)} verbs")
    
    # Convert to list of verbs and numpy array of embeddings
    verbs = list(embeddings.keys())
    embedding_vectors = np.array([embeddings[verb] for verb in verbs])
    
    # Calculate cosine similarity matrix
    print("Calculating similarity matrix...")
    similarity_matrix = cosine_similarity(embedding_vectors)
    
    # Generate clusters of similar verbs
    clusters = {}
    print("Generating verb clusters...")
    for i, verb in enumerate(tqdm(verbs)):
        # Find most similar verbs (excluding self)
        similar_indices = np.argsort(similarity_matrix[i])[::-1][1:max_similar+1]
        
        # Filter by threshold and create list of (verb, similarity) pairs
        similar_verbs = []
        for idx in similar_indices:
            similarity = similarity_matrix[i][idx]
            if similarity >= threshold:
                similar_verbs.append({
                    "verb": verbs[idx],
                    "similarity": float(similarity)
                })
        
        # Store cluster
        clusters[verb] = similar_verbs
    
    # Save clusters to output file
    print(f"Saving clusters to {output_file}")
    with open(output_file, 'w', encoding='utf-8') as file:
        json.dump(clusters, file, ensure_ascii=False, indent=2)
    
    # Print summary
    print(f"Successfully generated clusters for {len(clusters)} verbs")
    avg_cluster_size = sum(len(cluster) for cluster in clusters.values()) / len(clusters)
    print(f"Average cluster size: {avg_cluster_size:.2f} verbs")
    
    # Show a sample cluster
    sample_verbs = ["parler", "manger", "écrire", "voir", "aller"]
    found_sample = None
    for sample in sample_verbs:
        if sample in clusters:
            found_sample = sample
            break
    
    if found_sample:
        print(f"\nSample cluster for '{found_sample}':")
        for item in clusters[found_sample]:
            print(f"  {item['verb']}: {item['similarity']:.4f}")

if __name__ == "__main__":
    # Check arguments
    if len(sys.argv) != 3:
        print("Usage: python generate_verb_clusters.py EMBEDDINGS_FILE OUTPUT_FILE")
        sys.exit(1)
    
    # Get input and output files from command line arguments
    embeddings_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Generate clusters
    generate_verb_clusters(embeddings_file, output_file)
