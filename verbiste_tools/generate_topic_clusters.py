#!/usr/bin/env python3
"""
Generate topic-based verb clusters for pedagogical purposes.
Creates clusters suitable for verb drills and practice exercises.
"""

import json
import os
import sys
import numpy as np
from tqdm import tqdm
from sklearn.cluster import KMeans
from sklearn.manifold import TSNE
import matplotlib.pyplot as plt

def generate_topic_clusters(embeddings_file, output_file, num_clusters=20):
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
            "total_verbs": len(verbs)
        },
        "topic_labels": topic_labels,
        "topic_clusters": topic_clusters
    }
    
    # Save topic clusters to output file
    print(f"Saving topic clusters to {output_file}")
    with open(output_file, 'w', encoding='utf-8') as file:
        json.dump(result, file, ensure_ascii=False, indent=2)
    
    # Generate visualization of clusters
    try:
        visualize_clusters(embedding_vectors, cluster_labels, verbs, 
                          os.path.splitext(output_file)[0] + "_viz.png")
    except Exception as e:
        print(f"Visualization creation failed: {str(e)}")
    
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

def visualize_clusters(embeddings, labels, verbs, output_file):
    """Create a t-SNE visualization of the verb clusters."""
    print("Generating t-SNE visualization...")
    
    # Apply t-SNE for dimensionality reduction
    tsne = TSNE(n_components=2, random_state=42, perplexity=40)
    reduced_embeddings = tsne.fit_transform(embeddings)
    
    # Create the plot
    plt.figure(figsize=(12, 10))
    
    # Get unique labels and assign colors
    unique_labels = set(labels)
    colors = plt.cm.rainbow(np.linspace(0, 1, len(unique_labels)))
    
    # Plot each cluster
    for label, color in zip(unique_labels, colors):
        indices = labels == label
        plt.scatter(
            reduced_embeddings[indices, 0],
            reduced_embeddings[indices, 1],
            c=[color],
            label=f"Cluster {label}",
            alpha=0.6
        )
    
    # Add annotations for central verbs in each cluster
    for label in unique_labels:
        indices = np.where(labels == label)[0]
        # Find center of cluster
        center = np.mean(reduced_embeddings[indices], axis=0)
        # Find closest verb to center
        distances = np.linalg.norm(reduced_embeddings[indices] - center, axis=1)
        central_idx = indices[np.argmin(distances)]
        plt.annotate(
            verbs[central_idx],
            reduced_embeddings[central_idx],
            fontsize=9,
            fontweight='bold'
        )
    
    plt.title("t-SNE Visualization of French Verb Clusters")
    plt.legend(loc='upper right', bbox_to_anchor=(1.15, 1))
    plt.tight_layout()
    
    # Save the visualization
    plt.savefig(output_file, dpi=300)
    print(f"Visualization saved to {output_file}")

def generate_drill_files(topics_file, output_dir="drills"):
    """Generate Org-mode drill files for each topic cluster."""
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Load topic clusters
    print(f"Loading topic clusters from {topics_file}")
    with open(topics_file, 'r', encoding='utf-8') as file:
        topics_data = json.load(file)
    
    topic_clusters = topics_data["topic_clusters"]
    topic_labels = topics_data["topic_labels"]
    
    print(f"Generating drill files for {len(topic_clusters)} topics...")
    
    # Generate a drill file for each topic
    for cluster_id, verbs in topic_clusters.items():
        # Get representative verbs to name the file
        representative = topic_labels[cluster_id]["representative_verbs"][0]
        filename = os.path.join(output_dir, f"{representative}-drill.org")
        
        # Create the drill file content
        with open(filename, 'w', encoding='utf-8') as file:
            # Write header
            file.write(f"#+TITLE: French Verb Drill: {representative} Group\n")
            file.write("#+AUTHOR: Generated by verbiste.el\n\n")
            
            # Write introduction
            file.write("* Introduction\n\n")
            file.write(f"This drill focuses on verbs related to '{representative}'.\n")
            file.write("Practice conjugating these verbs in different tenses.\n\n")
            
            # List all verbs in this cluster
            file.write("* Verbs in this group\n\n")
            for idx, item in enumerate(verbs):
                file.write(f"{idx+1}. {item['verb']}\n")
            
            # Create drill section
            file.write("\n* Conjugation Drills\n\n")
            
            # Generate drill exercises for the top 10 verbs
            for item in verbs[:10]:
                verb = item["verb"]
                file.write(f"** {verb}\n\n")
                
                # Present tense drill
                file.write("*** Present Tense\n\n")
                file.write("| Subject | Conjugation |\n")
                file.write("|------+------------|\n")
                for subject in ["je", "tu", "il/elle", "nous", "vous", "ils/elles"]:
                    file.write(f"| {subject} | |\n")
                
                # Future tense drill
                file.write("\n*** Future Tense\n\n")
                file.write("| Subject | Conjugation |\n")
                file.write("|------+------------|\n")
                for subject in ["je", "tu", "il/elle", "nous", "vous", "ils/elles"]:
                    file.write(f"| {subject} | |\n")
                
                # Example sentences
                file.write("\n*** Example Sentences\n\n")
                file.write("1. [Create a sentence using this verb]\n")
                file.write("2. [Create a sentence using this verb in past tense]\n")
                file.write("3. [Create a sentence using this verb in future tense]\n\n")
        
        print(f"Created drill file: {filename}")
    
    print(f"Successfully generated {len(topic_clusters)} drill files in {output_dir}/")

if __name__ == "__main__":
    # Check arguments
    if len(sys.argv) < 3:
        print("Usage: python generate_topic_clusters.py EMBEDDINGS_FILE OUTPUT_FILE [NUM_CLUSTERS]")
        sys.exit(1)
    
    # Get input and output files from command line arguments
    embeddings_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Get optional number of clusters
    num_clusters = 20  # Default value
    if len(sys.argv) > 3:
        try:
            num_clusters = int(sys.argv[3])
        except ValueError:
            print(f"Invalid number of clusters: {sys.argv[3]}, using default: {num_clusters}")
    
    # Generate topic clusters
    generate_topic_clusters(embeddings_file, output_file, num_clusters)
    
    # Ask if drill files should be generated
    print("\nDo you want to generate drill files for these topic clusters? (y/n) ", end="")
    choice = input().strip().lower()
    if choice in ['y', 'yes']:
        generate_drill_files(output_file)