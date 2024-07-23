
from transformers import GPTNeoForCausalLM, GPT2Tokenizer

# Carregar o modelo e o tokenizer pré-treinados
model_name = "EleutherAI/gpt-neo-1.3B"
model = GPTNeoForCausalLM.from_pretrained(model_name)
tokenizer = GPT2Tokenizer.from_pretrained(model_name)

def generate_code(prompt):
    inputs = tokenizer(prompt, return_tensors="pt")
    outputs = model.generate(
        inputs["input_ids"], 
        max_length=150, 
        num_return_sequences=1,
        pad_token_id=tokenizer.eos_token_id,
        no_repeat_ngram_size=2,
        temperature=0.7,
        top_p=0.9,
        do_sample=True  # Ativa amostragem para gerar texto mais diverso
    )
    generated_code = tokenizer.decode(outputs[0], skip_special_tokens=True)
    return generated_code

if __name__ == "__main__":
    import sys
    prompt = sys.argv[1]
    print(generate_code(prompt))
