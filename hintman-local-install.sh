
#!/usr/bin/env bash
set -eo pipefail   # We may need to remove this. What if Nodejs/npm are already installed on the system? On MacOs it gives an `Error` which breaks dude to this flag. 

# Install dependencies for Smee (Node, Npm) and Ruby
apt-get install nodejs npm
apt-get install ruby-full
npm install --global smee-client
smee -u "https://smee.io/uTG0BCXnjq4DEff7"

# Initialize variables for GitHub App 
export GITHUB_APP_IDENTIFIER=12345
export GITHUB_WEBHOOK_SECRET="XXXXXXX" # This is optional, depends on whether we will be using the webhook_secret for our app
export GITHUB_PRIVATE_KEY=`awk '{printf "%s\\n", $0}' hintman-private-key.pem`


# Install dependencies for Sinatra app
gem install bundler
bundle install

ruby server.rb  #The file where the sinatra app is 