#!/usr/bin/env python3
"""
Generate clusters of similar French verbs based on embedding similarity.
This script reads embeddings of French verbs and generates clusters of similar verbs
based on cosine similarity between their embeddings.
"""

import json
import os
import click
import numpy as np
from tqdm import tqdm
from sklearn.metrics.pairwise import cosine_similarity

@click.command()
@click.argument('embeddings_file', type=click.Path(exists=True))
@click.argument('output_file', type=click.Path())
@click.option('--threshold', default=0.7, help='Similarity threshold for clustering (0.0 to 1.0)')
@click.option('--max-similar', default=10, help='Maximum number of similar verbs to include per verb')
def generate_clusters(embeddings_file, output_file, threshold, max_similar):
    """Generate clusters of similar French verbs based on embedding similarity.
    
    Args:
        embeddings_file: Path to the JSON file containing verb embeddings
        output_file: Path to save the JSON file with verb clusters
        threshold: Similarity threshold for clustering (0.0 to 1.0)
        max_similar: Maximum number of similar verbs to include per verb
    """
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file) or '.', exist_ok=True)
    
    # Load verb embeddings
    with open(embeddings_file, 'r', encoding='utf-8') as file:
        embeddings = json.load(file)
    
    click.echo(f"Loaded embeddings for {len(embeddings)} verbs")
    
    # Convert to list of verbs and list of embedding vectors
    verbs = list(embeddings.keys())
    embedding_vectors = np.array([embeddings[verb] for verb in verbs])
    
    # Calculate cosine similarity matrix
    click.echo("Calculating similarity matrix...")
    similarity_matrix = cosine_similarity(embedding_vectors)
    
    # Generate clusters of similar verbs
    clusters = {}
    click.echo("Generating verb clusters...")
    for i, verb in enumerate(tqdm(verbs)):
        # Sort similar verbs by similarity score (excluding self)
        similar_indices = np.argsort(similarity_matrix[i])[::-1][1:max_similar+1]
        
        # Filter by threshold and create list of (verb, similarity) tuples
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
    with open(output_file, 'w', encoding='utf-8') as file:
        json.dump(clusters, file, ensure_ascii=False, indent=2)
    
    # Print summary
    click.echo(f"Successfully generated clusters for {len(clusters)} verbs")
    click.echo(f"Saved to {os.path.abspath(output_file)}")
    
    # Show a sample cluster
    if clusters:
        sample_verb = next(iter(clusters))
        click.echo(f"\nSample cluster for '{sample_verb}':")
        for item in clusters[sample_verb][:5]:
            click.echo(f"  {item['verb']}: {item['similarity']:.4f}")

if __name__ == "__main__":
    generate_clusters()
