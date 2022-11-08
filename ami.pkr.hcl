# Create an image
# [reference documentation](https://www.packer.io/docs/templates)

# Execution:
# AWS_ACCESS_KEY_ID=$(aws configure get aws_access_key_id)
# AWS_SECRET_ACCESS_KEY=$(aws configure get aws_secret_access_key)
# packer build ami_gen_example.pkr.hcl


locals { timestamp = regex_replace(timestamp(), "[- TZ:]", "") }

# source blocks configure your builder plugins; your source is then used inside
# build blocks to create resources. A build block runs provisioners and
# post-processors on an instance created by the source.
source "amazon-ebs" "example" {
  ami_name      = "sieve_ami_exampleteam_ta1 ${local.timestamp}"
  instance_type = "t2.micro"
  region        = "us-east-1"
  source_ami_filter {
    filters = {
      name                = "amzn2-ami-hvm-*-x86_64-gp2"
    }
    most_recent = true
    # Indicate that only an ami from Amazon should be used
    owners      = ["137112412989"]
  }
  ssh_username = "ec2-user"
}

# a build block invokes sources and runs provisioning steps on them.
# https://www.elastic.co/guide/en/beats/metricbeat/current/setup-repositories.html
build {
  # this points to the base image specified above
  sources = ["source.amazon-ebs.example"]

  provisioner "shell-local" {
    inline = ["echo in_progress"]
  }

  provisioner "shell" {
    inline = [
      "sudo yum update -y",
      "sudo yum install -y curl git python3 python3-pip m4",
      "pip3 install pysmps numpy pmlb scikit-learn",
      "curl https://sh.rustup.rs -sSf | bash -s -- -y",
      "wget https://raw.githubusercontent.com/coin-or/coinbrew/master/coinbrew",
      "chmod u+x coinbrew",
      "./coinbrew fetch Cbc@master",
      "./coinbrew build Cbc",

      "mkdir /tmp/gen",      
      ""
    ]
  }
  
    
  provisioner "file" {
    source = "testcase-generation"
    destination = "/tmp/gen"
  }
  
  provisioner "file" {
    source = "rust-circ"
    destination = "/tmp/circ"
  }

  provisioner "file" {
    source = "codegen"
    destination = "/tmp/codegen"
  }

  
  provisioner "shell" {
    inline = [
      # mode into home
      "mv /tmp/gen ~/testcase-generation",
      "mv /tmp/circ ~/circ",
      "mv /tmp/codegen ~/codegen",    

      # Set permissions
      # "chmod 777 ~/generate_statements",
      "chmod -R 777 ~/testcase-generation",  
      "chmod -R 777 ~/circ",
      "chmod -R 777 ~/codegen",

      # build circ
      "cd /files/circ",
      "rustup override set stable",
      "cargo build --release --example circ --features c r1cs",
      ""
    ]
  }

}
