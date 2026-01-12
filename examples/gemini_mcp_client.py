#!/usr/bin/env python3

"""
ErlVectorDB + Gemini CLI MCP Client

This example demonstrates how to use ErlVectorDB as an MCP server with Google's Gemini AI
for intelligent vector operations, semantic search, and AI-powered data analysis.

Features:
- Connect to ErlVectorDB via MCP protocol
- Use Gemini AI for text embedding generation
- Semantic search with AI-generated queries
- Intelligent document analysis and categorization
- AI-powered vector similarity explanations

Requirements:
    pip install google-generativeai requests

Usage:
    python gemini_mcp_client.py

Environment Variables:
    GEMINI_API_KEY - Your Google AI API key
    ERLVECTORDB_HOST - MCP server host (default: localhost)
    ERLVECTORDB_PORT - MCP server port (default: 8080)
    ERLVECTORDB_CLIENT_ID - OAuth client ID (default: admin)
    ERLVECTORDB_CLIENT_SECRET - OAuth client secret (default: admin_secret_2024)
"""

import json
import socket
import requests
import time
import os
import sys
from typing import Dict, List, Any, Optional
import google.generativeai as genai

class GeminiMCPClient:
    def __init__(self, gemini_api_key=None, host=None, port=None, 
                 oauth_host=None, oauth_port=None, client_id=None, client_secret=None):
        # Gemini AI setup
        self.gemini_api_key = gemini_api_key or os.getenv('GEMINI_API_KEY')
        if not self.gemini_api_key:
            raise ValueError("GEMINI_API_KEY environment variable is required")
        
        genai.configure(api_key=self.gemini_api_key)
        self.gemini_model = genai.GenerativeModel('gemini-pro')
        
        # ErlVectorDB MCP setup
        self.host = host or os.getenv('ERLVECTORDB_HOST', 'localhost')
        self.port = int(port or os.getenv('ERLVECTORDB_PORT', '8080'))
        self.oauth_host = oauth_host or os.getenv('ERLVECTORDB_OAUTH_HOST', 'localhost')
        self.oauth_port = int(oauth_port or os.getenv('ERLVECTORDB_OAUTH_PORT', '8081'))
        self.client_id = client_id or os.getenv('ERLVECTORDB_CLIENT_ID', 'admin')
        self.client_secret = client_secret or os.getenv('ERLVECTORDB_CLIENT_SECRET', 'admin_secret_2024')
        
        # Connection state
        self.access_token = None
        self.socket = None
        self.request_id = 1
        self.connected = False

    def get_access_token(self) -> str:
        """Get OAuth access token from ErlVectorDB"""
        print("🔐 Getting OAuth access token...")
        
        url = f'http://{self.oauth_host}:{self.oauth_port}/oauth/token'
        data = {
            'grant_type': 'client_credentials',
            'client_id': self.client_id,
            'client_secret': self.client_secret,
            'scope': 'read write admin'
        }
        
        response = requests.post(url, data=data, timeout=10)
        response.raise_for_status()
        
        token_data = response.json()
        self.access_token = token_data['access_token']
        print("✅ OAuth token obtained")
        return self.access_token

    def connect_to_vectordb(self):
        """Connect to ErlVectorDB MCP server"""
        self.get_access_token()
        
        print(f"🔌 Connecting to ErlVectorDB at {self.host}:{self.port}...")
        
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.settimeout(10)
        self.socket.connect((self.host, self.port))
        self.connected = True
        
        # Initialize MCP connection
        self.send_mcp_request('initialize', {
            'protocolVersion': '2024-11-05',
            'capabilities': {'tools': {}},
            'clientInfo': {
                'name': 'gemini-mcp-client',
                'version': '1.0.0'
            }
        })
        
        print("✅ Connected to ErlVectorDB")

    def send_mcp_request(self, method: str, params: Dict = None) -> Dict:
        """Send MCP request to ErlVectorDB"""
        if not self.connected:
            raise Exception("Not connected to MCP server")

        request_id = self.request_id
        self.request_id += 1

        request = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params or {},
            'id': request_id,
            'auth': {
                'type': 'bearer',
                'token': self.access_token
            }
        }

        request_data = json.dumps(request).encode('utf-8')
        self.socket.send(request_data)

        # Receive response
        self.socket.settimeout(30)
        response_data = self.socket.recv(8192)
        response = json.loads(response_data.decode('utf-8'))

        if 'error' in response:
            raise Exception(f"MCP Error: {response['error']['message']}")

        return response.get('result', {})

    def call_tool(self, name: str, arguments: Dict) -> Dict:
        """Call ErlVectorDB tool via MCP"""
        return self.send_mcp_request('tools/call', {
            'name': name,
            'arguments': arguments
        })

    def generate_embedding(self, text: str) -> List[float]:
        """Generate text embedding using Gemini AI
        
        Note: This is a simplified embedding generation.
        In production, you'd use a dedicated embedding model like text-embedding-004
        or integrate with Vertex AI embeddings.
        """
        print(f"🧠 Generating embedding for: '{text[:50]}...'")
        
        # Use Gemini to generate a semantic representation
        # This is a simplified approach - in practice you'd use proper embedding models
        prompt = f"""
        Convert the following text into a numerical vector representation.
        Generate exactly 128 floating point numbers between -1 and 1 that represent
        the semantic meaning of this text. Return only the numbers separated by commas.
        
        Text: {text}
        
        Vector (128 numbers):
        """
        
        try:
            response = self.gemini_model.generate_content(prompt)
            # Parse the response to extract numbers
            numbers_text = response.text.strip()
            # Simple parsing - in production you'd use a proper embedding model
            numbers = [float(x.strip()) for x in numbers_text.split(',')[:128]]
            
            # Ensure we have exactly 128 dimensions
            while len(numbers) < 128:
                numbers.append(0.0)
            numbers = numbers[:128]
            
            print(f"✅ Generated {len(numbers)}-dimensional embedding")
            return numbers
            
        except Exception as e:
            print(f"⚠️  Embedding generation failed, using random vector: {e}")
            # Fallback to a simple hash-based approach
            import hashlib
            hash_obj = hashlib.md5(text.encode())
            hash_bytes = hash_obj.digest()
            # Convert to 128 floats between -1 and 1
            vector = []
            for i in range(128):
                byte_val = hash_bytes[i % len(hash_bytes)]
                normalized = (byte_val / 255.0) * 2 - 1  # Scale to [-1, 1]
                vector.append(normalized)
            return vector

    def analyze_with_gemini(self, text: str, context: str = "") -> Dict:
        """Use Gemini to analyze text and extract metadata"""
        print(f"🔍 Analyzing text with Gemini...")
        
        prompt = f"""
        Analyze the following text and provide structured information:
        
        Text: {text}
        Context: {context}
        
        Please provide a JSON response with:
        - title: A concise title for this text
        - category: The main category (tech, science, business, etc.)
        - keywords: List of 3-5 key terms
        - summary: One sentence summary
        - sentiment: positive, negative, or neutral
        - topics: List of main topics discussed
        
        Return only valid JSON:
        """
        
        try:
            response = self.gemini_model.generate_content(prompt)
            # Try to parse JSON from response
            response_text = response.text.strip()
            
            # Clean up the response to extract JSON
            if '```json' in response_text:
                json_start = response_text.find('```json') + 7
                json_end = response_text.find('```', json_start)
                response_text = response_text[json_start:json_end]
            elif '```' in response_text:
                json_start = response_text.find('```') + 3
                json_end = response_text.find('```', json_start)
                response_text = response_text[json_start:json_end]
            
            analysis = json.loads(response_text)
            print("✅ Text analysis completed")
            return analysis
            
        except Exception as e:
            print(f"⚠️  Analysis failed: {e}")
            # Fallback analysis
            return {
                "title": text[:50] + "..." if len(text) > 50 else text,
                "category": "general",
                "keywords": ["text", "document"],
                "summary": "Document content",
                "sentiment": "neutral",
                "topics": ["general"]
            }

    def explain_similarity(self, query: str, results: List) -> str:
        """Use Gemini to explain why certain results are similar"""
        print("🤔 Generating similarity explanation...")
        
        result_summaries = []
        for i, (doc_id, metadata, distance) in enumerate(results[:3]):
            result_summaries.append(f"Result {i+1}: {metadata.get('title', doc_id)} (distance: {distance:.3f})")
        
        prompt = f"""
        Explain why these search results are relevant to the query in simple terms:
        
        Query: {query}
        
        Results:
        {chr(10).join(result_summaries)}
        
        Provide a brief, friendly explanation of why these results match the query.
        Focus on semantic similarity and relevance.
        """
        
        try:
            response = self.gemini_model.generate_content(prompt)
            explanation = response.text.strip()
            print("✅ Similarity explanation generated")
            return explanation
        except Exception as e:
            print(f"⚠️  Explanation generation failed: {e}")
            return "These results were found based on vector similarity matching."

    def smart_search(self, query: str, store_name: str, k: int = 5) -> Dict:
        """Perform AI-enhanced semantic search"""
        print(f"🔍 Performing smart search for: '{query}'")
        
        # Generate embedding for the query
        query_vector = self.generate_embedding(query)
        
        # Search in vector database
        search_result = self.call_tool('search_vectors', {
            'store': store_name,
            'vector': query_vector,
            'k': k
        })
        
        # Parse results
        results = json.loads(search_result['content'][0]['text'])
        
        # Generate explanation using Gemini
        explanation = self.explain_similarity(query, results)
        
        return {
            'query': query,
            'results': results,
            'explanation': explanation,
            'result_count': len(results)
        }

    def smart_insert(self, store_name: str, doc_id: str, text: str) -> Dict:
        """Insert document with AI-generated embedding and metadata"""
        print(f"📝 Smart inserting document: {doc_id}")
        
        # Generate embedding
        vector = self.generate_embedding(text)
        
        # Analyze text with Gemini
        analysis = self.analyze_with_gemini(text)
        
        # Add original text to metadata
        metadata = analysis.copy()
        metadata['original_text'] = text[:500] + "..." if len(text) > 500 else text
        metadata['embedding_model'] = 'gemini-generated'
        metadata['inserted_at'] = time.time()
        
        # Insert into vector database
        insert_result = self.call_tool('insert_vector', {
            'store': store_name,
            'id': doc_id,
            'vector': vector,
            'metadata': metadata
        })
        
        return {
            'doc_id': doc_id,
            'analysis': analysis,
            'vector_dimensions': len(vector),
            'insert_result': insert_result
        }

    def disconnect(self):
        """Disconnect from ErlVectorDB"""
        if self.socket:
            self.socket.close()
            self.socket = None
        self.connected = False

def run_gemini_demo():
    """Run the Gemini + ErlVectorDB demonstration"""
    print("🚀 Gemini + ErlVectorDB MCP Demo")
    print("=" * 40)
    
    # Check for Gemini API key
    if not os.getenv('GEMINI_API_KEY'):
        print("❌ GEMINI_API_KEY environment variable is required")
        print("Get your API key from: https://makersuite.google.com/app/apikey")
        return
    
    client = GeminiMCPClient()
    
    try:
        # Connect to ErlVectorDB
        client.connect_to_vectordb()
        
        # Create a test store
        store_name = 'gemini_demo_store'
        print(f"\n🏪 Creating store: {store_name}")
        client.call_tool('create_store', {'name': store_name})
        
        # Sample documents to insert
        documents = [
            {
                'id': 'ai_article',
                'text': 'Artificial intelligence is revolutionizing healthcare by enabling faster diagnosis, personalized treatment plans, and drug discovery. Machine learning algorithms can analyze medical images with superhuman accuracy.'
            },
            {
                'id': 'climate_report', 
                'text': 'Climate change is causing rising sea levels, extreme weather events, and ecosystem disruption. Renewable energy technologies like solar and wind power are crucial for reducing carbon emissions.'
            },
            {
                'id': 'space_exploration',
                'text': 'Space exploration has led to numerous technological innovations including GPS, satellite communications, and advanced materials. Mars missions are planned to search for signs of past or present life.'
            },
            {
                'id': 'quantum_computing',
                'text': 'Quantum computing promises to solve complex problems that are intractable for classical computers. Applications include cryptography, drug discovery, and optimization problems in logistics.'
            }
        ]
        
        # Insert documents with AI analysis
        print("\n📚 Inserting documents with AI analysis...")
        for doc in documents:
            result = client.smart_insert(store_name, doc['id'], doc['text'])
            print(f"  ✅ {doc['id']}: {result['analysis']['title']}")
            print(f"     Category: {result['analysis']['category']}")
            print(f"     Keywords: {', '.join(result['analysis']['keywords'])}")
        
        # Perform smart searches
        print("\n🔍 Performing AI-enhanced searches...")
        
        search_queries = [
            "medical technology and healthcare",
            "environmental issues and solutions", 
            "future technology and computing",
            "space and scientific discovery"
        ]
        
        for query in search_queries:
            print(f"\n🔍 Query: '{query}'")
            search_result = client.smart_search(query, store_name, k=3)
            
            print(f"📊 Found {search_result['result_count']} results:")
            for i, (doc_id, metadata, distance) in enumerate(search_result['results']):
                print(f"  {i+1}. {metadata.get('title', doc_id)} (similarity: {1-distance:.3f})")
                print(f"     Category: {metadata.get('category', 'unknown')}")
            
            print(f"💡 AI Explanation: {search_result['explanation']}")
        
        # Demonstrate category-based analysis
        print("\n📊 Document Analysis Summary:")
        
        # Get all documents and analyze patterns
        categories = {}
        sentiments = {}
        
        for doc in documents:
            analysis = client.analyze_with_gemini(doc['text'])
            category = analysis.get('category', 'unknown')
            sentiment = analysis.get('sentiment', 'neutral')
            
            categories[category] = categories.get(category, 0) + 1
            sentiments[sentiment] = sentiments.get(sentiment, 0) + 1
        
        print(f"📈 Categories: {dict(categories)}")
        print(f"😊 Sentiments: {dict(sentiments)}")
        
        # Sync and backup
        print(f"\n💾 Syncing store...")
        client.call_tool('sync_store', {'store': store_name})
        
        backup_name = f'gemini_demo_backup_{int(time.time())}'
        print(f"💼 Creating backup: {backup_name}")
        client.call_tool('backup_store', {
            'store': store_name,
            'backup_name': backup_name
        })
        
        print("\n✅ Gemini + ErlVectorDB demo completed successfully!")
        print("\n🎯 Key Features Demonstrated:")
        print("  • AI-generated text embeddings")
        print("  • Semantic search with explanations")
        print("  • Automated document analysis")
        print("  • Intelligent metadata extraction")
        print("  • Vector similarity with AI insights")
        
    except Exception as e:
        print(f"\n❌ Demo failed: {e}")
        import traceback
        traceback.print_exc()
    finally:
        client.disconnect()

if __name__ == '__main__':
    print("🤖 ErlVectorDB + Google Gemini MCP Integration")
    print("=" * 50)
    
    # Check requirements
    try:
        import google.generativeai as genai
    except ImportError:
        print("❌ Missing required package: google-generativeai")
        print("Install with: pip install google-generativeai")
        sys.exit(1)
    
    try:
        run_gemini_demo()
        print("\n👋 Demo finished")
    except KeyboardInterrupt:
        print("\n⚠️  Interrupted by user")
    except Exception as e:
        print(f"\n💥 Fatal error: {e}")
        sys.exit(1)