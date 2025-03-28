#!/usr/bin/env python3
import requests
import json
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.decomposition import PCA
import click
import os
import time
import sys

def get_embeddings(texts, model="nomic-embed-text", max_retries=3, retry_delay=2):
    """Get embeddings from Ollama API with retry logic"""
    embeddings = []
    successful_texts = []
    
    for text in texts:
        for attempt in range(max_retries):
            try:
                click.echo(f"Processing: {text}")
                response = requests.post(
                    "http://localhost:11434/api/embeddings",
                    json={
                        "model": model,
                        "prompt": text
                    },
                    timeout=10  # Add timeout to avoid hanging
                )
                
                if response.status_code == 200:
                    result = response.json()
                    embeddings.append(result["embedding"])
                    successful_texts.append(text)
                    break  # Success, exit retry loop
                else:
                    click.echo(f"Error for text '{text}': {response.text}", err=True)
                    if attempt < max_retries - 1:
                        click.echo(f"Retrying in {retry_delay} seconds...", err=True)
                        time.sleep(retry_delay)
            except (requests.exceptions.ConnectionError, 
                    requests.exceptions.Timeout,
                    requests.exceptions.RequestException) as e:
                click.echo(f"Request failed for '{text}': {str(e)}", err=True)
                if attempt < max_retries - 1:
                    click.echo(f"Retrying in {retry_delay} seconds...", err=True)
                    time.sleep(retry_delay)
                else:
                    click.echo(f"Max retries reached for '{text}'. Skipping.", err=True)
    
    return np.array(embeddings), successful_texts

def plot_similarity_matrix(matrix, labels, output_file):
    """Plot and save a similarity matrix as a heatmap"""
    plt.figure(figsize=(10, 8))
    plt.imshow(matrix, cmap='viridis')
    plt.colorbar()
    plt.xticks(np.arange(len(labels)), labels, rotation=45)
    plt.yticks(np.arange(len(labels)), labels)
    plt.title('Semantic Similarity Between French Verbs')
    plt.tight_layout()
    plt.savefig(output_file)
    plt.close()
    
def plot_pca(embeddings, labels, output_file):
    """Create and save a PCA plot of embeddings"""
    # Reduce to 2D for visualization
    pca = PCA(n_components=2)
    embeddings_2d = pca.fit_transform(embeddings)
    
    # Plot
    plt.figure(figsize=(10, 8))
    plt.scatter(embeddings_2d[:, 0], embeddings_2d[:, 1], s=100)
    
    # Add labels
    for i, label in enumerate(labels):
        plt.annotate(label, (embeddings_2d[i, 0], embeddings_2d[i, 1]), 
                     fontsize=12, ha='center', va='center')
    
    plt.title('PCA of French Verb Embeddings')
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.savefig(output_file)
    plt.close()

@click.command()
@click.option('--input-file', '-i', type=click.Path(exists=True), 
              help='JSON file containing a list of French verbs')
@click.option('--verbs', '-v', multiple=True, 
              help='Specify verbs directly (can be used multiple times)')
@click.option('--model', '-m', default='nomic-embed-text', 
              help='Embedding model to use from Ollama')
@click.option('--output', '-o', default='french_verb_similarity.png', 
              help='Output file for similarity matrix visualization')
@click.option('--pca', is_flag=True, 
              help='Create PCA visualization of embeddings')
@click.option('--save-data', '-s', is_flag=True, 
              help='Save embeddings and similarity matrix')
@click.option('--max-retries', default=3, type=int,
              help='Maximum number of retry attempts for API calls')
def main(input_file, verbs, model, output, pca, save_data, max_retries):
    """Analyze semantic similarities between French verbs using embeddings from Ollama."""
    
    # Get verbs from file, command line, or use defaults
    if input_file and os.path.exists(input_file):
        with open(input_file, 'r') as f:
            french_verbs = json.load(f)
        click.echo(f"Loaded {len(french_verbs)} verbs from {input_file}")
    elif verbs:
        french_verbs = list(verbs)
        click.echo(f"Using {len(french_verbs)} verbs provided via command line")
    else:
        # Example French verbs as default
        french_verbs = [
            "aller", "venir", "faire", "dire", "voir", 
            "savoir", "pouvoir", "vouloir", "devoir", "prendre",
            "parler", "manger", "boire", "dormir", "courir"
        ]
        click.echo(f"Using {len(french_verbs)} default example verbs")
    
    # Get embeddings with retry logic
    click.echo(f"Getting embeddings for {len(french_verbs)} verbs using model {model}...")
    embeddings, successful_verbs = get_embeddings(french_verbs, model, max_retries)
    
    if len(embeddings) == 0:
        click.echo("Failed to get any embeddings. Exiting.", err=True)
        sys.exit(1)
    
    # Let user know if some verbs failed
    if len(successful_verbs) < len(french_verbs):
        click.echo(f"Warning: Only obtained embeddings for {len(successful_verbs)}/{len(french_verbs)} verbs.")
        # Update the verb list to only include successful ones
        french_verbs = successful_verbs
    
    # Calculate similarity matrix
    click.echo("Calculating similarity matrix...")
    sim_matrix = cosine_similarity(embeddings)
    
    # Plot similarity matrix
    base_filename = os.path.splitext(output)[0]
    click.echo(f"Plotting similarity matrix and saving to {output}...")
    plot_similarity_matrix(sim_matrix, french_verbs, output)
    
    # Optionally create PCA plot
    if pca:
        pca_file = f"{base_filename}_pca.png"
        click.echo(f"Creating PCA visualization and saving to {pca_file}...")
        plot_pca(embeddings, french_verbs, pca_file)
    
    # Save data if requested
    if save_data:
        # Save similarity matrix
        matrix_file = f"{base_filename}_matrix.npy"
        np.save(matrix_file, sim_matrix)
        click.echo(f"Similarity matrix saved to {matrix_file}")
        
        # Save embeddings
        embeddings_file = f"{base_filename}_embeddings.npy"
        np.save(embeddings_file, embeddings)
        click.echo(f"Raw embeddings saved to {embeddings_file}")
        
        # Save list of successful verbs
        verbs_file = f"{base_filename}_verbs.json"
        with open(verbs_file, 'w') as f:
            json.dump(french_verbs, f, indent=2)
        click.echo(f"Verb list saved to {verbs_file}")
    
    click.echo("Done!")

if __name__ == "__main__":
    main()
