#!/usr/bin/env python3
"""
Generate topic-based verb clusters for pedagogical purposes using a small subset of verbs.
Creates clusters suitable for verb drills and practice exercises.
"""

import json
import os
import sys
import numpy as np
from sklearn.cluster import KMeans
import random

def generate_topic_clusters(embeddings_file, output_file, num_clusters=15, max_verbs=500):
    """Generate topic-based clusters of French verbs for pedagogical purposes."""
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file) or '.', exist_ok=True)
    
    # Load verb embeddings
    print(f"Loading embeddings from {embeddings_file}")
    with open(embeddings_file, 'r', encoding='utf-8') as file:
        embeddings = json.load(file)
    
    print(f"Loaded embeddings for {len(embeddings)} verbs")
    
    # Convert to list of verbs and numpy array of embeddings
    verbs = list(embeddings.keys())
    
    # Sample a subset of verbs for faster processing
    if len(verbs) > max_verbs:
        print(f"Sampling {max_verbs} verbs for faster processing")
        selected_indices = random.sample(range(len(verbs)), max_verbs)
        selected_verbs = [verbs[i] for i in selected_indices]
        verbs = selected_verbs
    
    embedding_vectors = np.array([embeddings[verb] for verb in verbs])
    
    # Apply K-means clustering
    print(f"Performing K-means clustering with {num_clusters} clusters...")
    kmeans = KMeans(n_clusters=num_clusters, random_state=42, n_init=10)
    cluster_labels = kmeans.fit_predict(embedding_vectors)
    
    # Group verbs by cluster
    topic_clusters = {i: [] for i in range(num_clusters)}
    for i, verb in enumerate(verbs):
        cluster_id = int(cluster_labels[i])
        
        # Calculate distance to cluster center (for sorting within clusters)
        center_distance = np.linalg.norm(embedding_vectors[i] - kmeans.cluster_centers_[cluster_id])
        
        topic_clusters[cluster_id].append({
            "verb": verb,
            "centrality": float(1.0 / (1.0 + center_distance))  # Higher value = closer to center
        })
    
    # Sort verbs within each cluster by centrality (most central first)
    for cluster_id in topic_clusters:
        topic_clusters[cluster_id].sort(key=lambda x: x["centrality"], reverse=True)
    
    # Generate topic labels (placeholder - in a real implementation, these would be generated 
    # based on the cluster contents or manually assigned)
    topic_labels = {}
    for cluster_id, verbs in topic_clusters.items():
        # Use most central 5 verbs to represent the cluster
        representative_verbs = [v["verb"] for v in verbs[:5]]
        topic_labels[cluster_id] = {
            "representative_verbs": representative_verbs,
            "verb_count": len(verbs)
        }
    
    # Create final output structure
    result = {
        "meta": {
            "num_clusters": num_clusters,
            "total_verbs": len(verbs),
            "max_verbs": max_verbs
        },
        "topic_labels": topic_labels,
        "topic_clusters": topic_clusters
    }
    
    # Save topic clusters to output file
    print(f"Saving topic clusters to {output_file}")
    with open(output_file, 'w', encoding='utf-8') as file:
        json.dump(result, file, ensure_ascii=False, indent=2)
    
    # Print summary
    print(f"Successfully generated {num_clusters} topic clusters")
    cluster_sizes = [len(topic_clusters[i]) for i in range(num_clusters)]
    print(f"Average cluster size: {np.mean(cluster_sizes):.1f} verbs")
    print(f"Cluster size range: {min(cluster_sizes)} to {max(cluster_sizes)} verbs")
    
    # Print example clusters
    print("\nExample topic clusters:")
    for i in range(min(3, num_clusters)):
        verbs_in_cluster = [item["verb"] for item in topic_clusters[i][:8]]
        print(f"Cluster {i}: {', '.join(verbs_in_cluster)}, ...")

if __name__ == "__main__":
    # Check arguments
    if len(sys.argv) < 3:
        print("Usage: python generate_topic_clusters_small.py EMBEDDINGS_FILE OUTPUT_FILE [NUM_CLUSTERS]")
        sys.exit(1)
    
    # Get input and output files from command line arguments
    embeddings_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Get optional number of clusters
    num_clusters = 15  # Default value
    if len(sys.argv) > 3:
        try:
            num_clusters = int(sys.argv[3])
        except ValueError:
            print(f"Invalid number of clusters: {sys.argv[3]}, using default: {num_clusters}")
    
    # Generate topic clusters
    generate_topic_clusters(embeddings_file, output_file, num_clusters)