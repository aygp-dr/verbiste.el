#!/bin/sh
# Check if embedding files exist
if [ ! -f "french_verb_similarity_embeddings.npy" ] || [ ! -f "french_verb_similarity_verbs.json" ]; then
    echo "Embedding files not found. Run the embedding analysis first."
    exit 1
fi

# Run clustering with different methods and cluster counts
python verbiste_tools/cluster_verbs.py \
    -e french_verb_similarity_embeddings.npy \
    -v french_verb_similarity_verbs.json \
    -n 3 \
    -m kmeans \
    -o french_verb_kmeans_clusters.png

python verbiste_tools/cluster_verbs.py \
    -e french_verb_similarity_embeddings.npy \
    -v french_verb_similarity_verbs.json \
    -n 4 \
    -m hierarchical \
    -o french_verb_hierarchical_clusters.png

echo "Clustering complete! Check the output files for results."
